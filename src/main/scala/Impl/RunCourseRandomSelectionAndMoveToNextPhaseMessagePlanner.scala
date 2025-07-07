package Impl

import Utils.SemesterPhaseProcess.validateAdminToken
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.TimePeriod
import Objects.SystemLogService.SystemLogEntry
import Utils.SemesterPhaseProcess.recordAdminOperationLog
import APIs.SemesterPhaseService.UpdateSemesterPhasePermissionsMessage
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.CourseInfo
import Objects.SemesterPhaseService.Permissions
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.ParameterList
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import cats.syntax.all.*
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe.generic.auto.deriveEncoder

case class RunCourseRandomSelectionAndMoveToNextPhaseMessagePlanner(
    adminToken: String,
    override val planContext: PlanContext
) extends Planner[String] {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[String] = {
    for {
      // Step 1: Validate admin token
      isValidToken <- validateAdminToken(adminToken)
      _ <- if (!isValidToken) IO.raiseError(new RuntimeException("无效的管理员Token")) else IO(logger.info("管理员Token验证成功"))

      // Step 2: Validate current phase is Phase1
      currentPhase <- getCurrentPhase
      _ <- if (currentPhase != 1) IO.raiseError(new RuntimeException("当前为阶段2")) else IO(logger.info("当前阶段为Phase1"))

      // Step 3: Confirm that all permissions are closed
      arePermissionsClosed <- checkPermissionsClosed
      _ <- if (!arePermissionsClosed) IO.raiseError(new RuntimeException("抽签前需关闭所有权限")) else IO(logger.info("所有权限已关闭"))

      // Step 4: Perform random selection logic
      _ <- performRandomSelection()

      // Step 5: Clear preselection pool
      _ <- clearPreselectionPool()

      // Step 6: Switch to Phase2 and close permissions
      _ <- moveToPhase2AndClosePermissions()

      // Step 7: Log the operation
      _ <- recordAdminOperationLog("Perform Course Random Selection", "抽签成功并阶段切换完成！")
    } yield "抽签成功并阶段切换完成！"
  }

  private def getCurrentPhase(using PlanContext): IO[Int] = {
    val query = s"SELECT current_phase FROM ${schemaName}.semester_phase_table LIMIT 1;"
    readDBInt(query, List.empty)
  }

  private def checkPermissionsClosed(using PlanContext): IO[Boolean] = {
    val query =
      s"""
         SELECT NOT (allow_teacher_manage OR allow_student_select OR allow_student_drop OR allow_student_evaluate)
         AS permissions_closed
         FROM ${schemaName}.semester_phase_table LIMIT 1;
       """
    readDBBoolean(query, List.empty)
  }

  private def performRandomSelection()(using PlanContext): IO[Unit] = {
    for {
      courseIDs <- getAllCourseIDs()
      _ <- IO(logger.info(s"共读取到${courseIDs.length}门课程ID"))
      courses <- courseIDs.map(getCourseDetailsByID).sequence
      _ <- IO(logger.info(s"查询到具体课程信息，共 ${courses.length} 门课程"))
      _ <- courses.map(randomlyAssignStudents).sequence_
    } yield ()
  }

  private def getAllCourseIDs()(using PlanContext): IO[List[Int]] = {
    val query = s"SELECT course_id FROM ${schemaName}.course_table;"
    readDBRows(query, List.empty).map(_.map(decodeField[Int](_, "course_id")))
  }

  private def getCourseDetailsByID(courseID: Int)(using PlanContext): IO[CourseInfo] = {
    import APIs.CourseManagementService.QueryCourseByIDMessage
    QueryCourseByIDMessage(adminToken, courseID).send
  }

  private def randomlyAssignStudents(course: CourseInfo)(using PlanContext): IO[Unit] = {
    for {
      students <- getPreselectedStudents(course.courseID)
      _ <- IO(logger.info(s"课程ID：${course.courseID}, 总学生数：${students.length}"))
      shuffled <- IO(scala.util.Random.shuffle(students))
      (selected, waiting) = shuffled.splitAt(course.courseCapacity.min(students.size))
      _ <- if (selected.isEmpty) IO(logger.info("没有学生选课，跳过selection插入操作"))
           else insertIntoCourseSelection(course.courseID, selected)
      _ <- if (waiting.isEmpty) IO(logger.info("没有学生进入等待名单，跳过waiting插入操作")) 
           else insertIntoWaitingList(course.courseID, waiting)
    } yield ()
  }

  private def getPreselectedStudents(courseID: Int)(using PlanContext): IO[List[Int]] = {
    val query = s"SELECT student_id FROM ${schemaName}.course_preselection_table WHERE course_id = ?;"
    readDBRows(query, List(SqlParameter("Int", courseID.toString))).map(_.map(decodeField[Int](_, "student_id")))
  }

  private def insertIntoCourseSelection(courseID: Int, students: List[Int])(using PlanContext): IO[Unit] = {
    val query = s"INSERT INTO ${schemaName}.course_selection_table (course_id, student_id) VALUES (?, ?);"
    val params = students.map(userId => ParameterList(List(SqlParameter("Int", courseID.toString), SqlParameter("Int", userId.toString))))
    writeDBList(query, params).void
  }

  private def insertIntoWaitingList(courseID: Int, students: List[Int])(using PlanContext): IO[Unit] = {
    val query = s"INSERT INTO ${schemaName}.waiting_list_table (course_id, student_id, position) VALUES (?, ?, ?);"
    val params = students.zipWithIndex.map { case (userId, position) =>
      ParameterList(List(
        SqlParameter("Int", courseID.toString),
        SqlParameter("Int", userId.toString),
        SqlParameter("Int", (position + 1).toString)
      ))
    }
    writeDBList(query, params).void
  }

  private def clearPreselectionPool()(using PlanContext): IO[Unit] = {
    val query = s"DELETE FROM ${schemaName}.course_preselection_table;"
    writeDB(query, List.empty).void
  }

  private def moveToPhase2AndClosePermissions()(using PlanContext): IO[Unit] = {
    for {
      _ <- updateCurrentPhase(2) // Switch to Phase2
      _ <- UpdateSemesterPhasePermissionsMessage(
        adminToken,
        allowTeacherManage = false,
        allowStudentSelect = false,
        allowStudentDrop = false,
        allowStudentEvaluate = false
      ).send
    } yield ()
  }

  private def updateCurrentPhase(phase: Int)(using PlanContext): IO[Unit] = {
    val query = s"UPDATE ${schemaName}.semester_phase_table SET current_phase = ?;"
    writeDB(query, List(SqlParameter("Int", phase.toString))).void
  }
}