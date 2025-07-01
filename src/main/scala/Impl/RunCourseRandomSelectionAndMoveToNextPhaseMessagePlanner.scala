package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Utils.SemesterPhaseProcess.validateAdminToken
import Utils.SemesterPhaseProcess.recordAdminOperationLog
import APIs.SemesterPhaseService.UpdateSemesterPhasePermissionsMessage
import Objects.CourseManagementService.{CourseInfo}
import Objects.SemesterPhaseService.Permissions
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.effect.IO
import org.slf4j.LoggerFactory
import cats.implicits._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.DBAPI._
import Common.API.{PlanContext, Planner}
import cats.effect.IO
import Common.Object.SqlParameter
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.ServiceUtils.schemaName
import Objects.CourseManagementService.CourseTime
import Objects.CourseManagementService.TimePeriod
import Objects.SystemLogService.SystemLogEntry
import Objects.CourseManagementService.DayOfWeek
import Objects.CourseManagementService.CourseInfo
import Objects.SemesterPhaseService.Permissions
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class RunCourseRandomSelectionAndMoveToNextPhaseMessagePlanner(
  adminToken: String,
  override val planContext: PlanContext
) extends Planner[String] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[String] = {
    for {
      // Step 1: 验证管理员令牌
      _ <- IO(logger.info(s"验证管理员令牌 ${adminToken}"))
      isAdminValid <- validateAdminToken(adminToken)
      _ <- if (!isAdminValid) IO.raiseError(new IllegalArgumentException("管理员令牌无效")) else IO.unit

      // Step 2: 验证当前阶段是否为 Phase1
      currentPhase <- getCurrentSemesterPhase()
      _ <- IO(logger.info(s"当前学期阶段为 ${currentPhase}"))
      _ <- if (currentPhase != 1) IO.raiseError(new IllegalStateException("当前为阶段2")) else IO.unit

      // Step 3: 确认当前阶段权限是否全部关闭
      arePermissionsClosed <- checkPermissionsClosed()
      _ <- IO(logger.info(s"当前学期阶段权限关闭状态: ${arePermissionsClosed}"))
      _ <- if (!arePermissionsClosed) IO.raiseError(new IllegalStateException("抽签前需关闭所有权限")) else IO.unit

      // Step 4: 执行抽签逻辑
      _ <- IO(logger.info("开始执行抽签逻辑"))
      _ <- executeRandomSelection()

      // Step 5: 清空预选池
      _ <- IO(logger.info("开始清空预选池"))
      _ <- clearPreselectionPool()

      // Step 6: 切换至阶段2并关闭相关权限
      _ <- IO(logger.info("切换至阶段2并关闭相关权限"))
      _ <- moveToPhase2()

      // Step 7: 记录操作日志
      operationLogDetails = s"管理员 ${adminToken} 完成抽签并阶段切换"
      _ <- recordAdminOperationLog("RunCourseRandomSelectionAndMoveToNextPhase", operationLogDetails)
      _ <- IO(logger.info("操作日志记录成功"))

    } yield {
      logger.info("抽签成功并阶段切换完成")
      "抽签成功并阶段切换完成！"
    }
  }

  private def getCurrentSemesterPhase()(using PlanContext): IO[Int] = {
    readDBInt(
      s"SELECT current_phase FROM ${schemaName}.semester_phase_table;",
      List.empty
    )
  }

  private def checkPermissionsClosed()(using PlanContext): IO[Boolean] = {
    for {
      allowTeacherManage <- readDBBoolean(s"SELECT allow_teacher_manage FROM ${schemaName}.semester_phase_table;", List.empty)
      allowStudentSelect <- readDBBoolean(s"SELECT allow_student_select FROM ${schemaName}.semester_phase_table;", List.empty)
      allowStudentDrop <- readDBBoolean(s"SELECT allow_student_drop FROM ${schemaName}.semester_phase_table;", List.empty)
      allowStudentEvaluate <- readDBBoolean(s"SELECT allow_student_evaluate FROM ${schemaName}.semester_phase_table;", List.empty)
    } yield !(allowTeacherManage || allowStudentSelect || allowStudentDrop || allowStudentEvaluate)
  }

  private def executeRandomSelection()(using PlanContext): IO[Unit] = {
    for {
      courses <- readDBRows(
        s"SELECT * FROM ${schemaName}.course_info;",
        List.empty
      )
      courseInfos = courses.map(decodeType[CourseInfo])

      _ <- courseInfos.traverse { course =>
        for {
          preselectedStudents <- readDBRows(
            s"SELECT user_id FROM ${schemaName}.course_preselection_table WHERE course_id = ?;",
            List(SqlParameter("Int", course.courseID.toString))
          )
          studentIDs = preselectedStudents.map(json => decodeField[Int](json, "user_id"))
          shuffledStudents = scala.util.Random.shuffle(studentIDs)
          selected = shuffledStudents.take(course.courseCapacity)
          waitingList = shuffledStudents.drop(course.courseCapacity)

          _ <- writeDBList(
            s"INSERT INTO ${schemaName}.course_selection_table (course_id, user_id) VALUES (?, ?);",
            selected.map(userID => ParameterList(List(
              SqlParameter("Int", course.courseID.toString),
              SqlParameter("Int", userID.toString)
            )))
          )
          _ <- writeDBList(
            s"INSERT INTO ${schemaName}.waiting_list_table (course_id, user_id, position) VALUES (?, ?, ?);",
            waitingList.zipWithIndex.map { case (userID, position) =>
              ParameterList(List(
                SqlParameter("Int", course.courseID.toString),
                SqlParameter("Int", userID.toString),
                SqlParameter("Int", position.toString)
              ))
            }
          )
        } yield ()
      }
    } yield ()
  }

  private def clearPreselectionPool()(using PlanContext): IO[Unit] = {
    writeDB(
      s"DELETE FROM ${schemaName}.course_preselection_table;",
      List.empty
    ).void
  }

  private def moveToPhase2()(using PlanContext): IO[Unit] = {
    for {
      _ <- writeDB(
        s"UPDATE ${schemaName}.semester_phase_table SET current_phase = 2;",
        List.empty
      )
      _ <- UpdateSemesterPhasePermissionsMessage(
        adminToken,
        allowTeacherManage = false,
        allowStudentSelect = false,
        allowStudentDrop = false,
        allowStudentEvaluate = false
      ).send.void
    } yield ()
  }
}