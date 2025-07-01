package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.SemesterPhaseService.Permissions
import Objects.SystemLogService.SystemLogEntry
import Utils.SemesterPhaseProcess.{validateAdminToken, recordAdminOperationLog}
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe.Json
import io.circe._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
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
import Utils.SemesterPhaseProcess.validateAdminToken
import Utils.SemesterPhaseProcess.recordAdminOperationLog
import io.circe.syntax._
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Utils.SemesterPhaseProcess.recordAdminOperationLog

case class UpdateSemesterPhasePermissionsMessagePlanner(
                                                         adminToken: String,
                                                         allowTeacherManage: Boolean,
                                                         allowStudentSelect: Boolean,
                                                         allowStudentDrop: Boolean,
                                                         allowStudentEvaluate: Boolean,
                                                         override val planContext: PlanContext
                                                       ) extends Planner[Permissions] {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[Permissions] = {
    for {
      // Step 1: Validate adminToken
      _ <- IO(logger.info(s"验证管理员 Token: ${adminToken}"))
      isAdminTokenValid <- validateAdminToken(adminToken)
      _ <- if (!isAdminTokenValid) IO.raiseError(new IllegalArgumentException("管理员 Token 无效")) else IO(logger.info("管理员 Token 验证通过"))

      // Step 2: Ensure the permissions are allowed in the current phase
      _ <- IO(logger.info("检查阶段权限是否允许"))
      currentPhase <- getCurrentPhase
      _ <- validatePermissions(currentPhase)

      // Step 3: Update permissions in the database
      _ <- IO(logger.info("更新学期阶段权限"))
      _ <- updatePhasePermissions()

      // Step 4: Retrieve updated permissions and return as a Permissions object
      _ <- IO(logger.info("获取更新后的学期阶段权限"))
      updatedPermissions <- getUpdatedPermissions()

      // Step 5: Record the admin operation log
      _ <- IO(logger.info("记录管理员权限变更操作日志"))
      _ <- recordAdminOperationLog(
        "UpdatePermissions",
        s"允许教师管理: ${allowTeacherManage}, 允许学生选课: ${allowStudentSelect}, 允许学生退课: ${allowStudentDrop}, 允许学生评价: ${allowStudentEvaluate}"
      )

    } yield updatedPermissions
  }

  private def getCurrentPhase(using PlanContext): IO[Int] = {
    val sql = s"SELECT current_phase FROM ${schemaName}.semester_phase_table LIMIT 1;"
    for {
      _ <- IO(logger.info("从数据库中读取当前阶段"))
      currentPhase <- readDBInt(sql, List())
    } yield currentPhase
  }

  private def validatePermissions(currentPhase: Int)(using PlanContext): IO[Unit] = {
    IO {
      logger.info(s"当前阶段: ${currentPhase}")
      if (currentPhase == 1 && allowStudentEvaluate) {
        logger.error("试图开启当前阶段不允许的权限: allowStudentEvaluate")
        throw new IllegalArgumentException("试图开启当前阶段不允许的权限")
      }
      if (currentPhase == 2 && allowTeacherManage) {
        logger.error("试图开启当前阶段不允许的权限: allowTeacherManage")
        throw new IllegalArgumentException("试图开启当前阶段不允许的权限")
      }
    }
  }

  private def updatePhasePermissions()(using PlanContext): IO[Unit] = {
    val sql =
      s"""
UPDATE ${schemaName}.semester_phase_table 
SET 
  allow_teacher_manage = ?, 
  allow_student_select = ?, 
  allow_student_drop = ?, 
  allow_student_evaluate = ?
WHERE current_phase = (SELECT current_phase FROM ${schemaName}.semester_phase_table LIMIT 1);
       """.stripMargin

    val parameters = List(
      SqlParameter("Boolean", allowTeacherManage.asJson.noSpaces),
      SqlParameter("Boolean", allowStudentSelect.asJson.noSpaces),
      SqlParameter("Boolean", allowStudentDrop.asJson.noSpaces),
      SqlParameter("Boolean", allowStudentEvaluate.asJson.noSpaces)
    )

    for {
      _ <- IO(logger.info("准备执行数据库更新语句"))
      result <- writeDB(sql, parameters)
      _ <- IO(logger.info(s"数据库更新成功，结果: ${result}"))
    } yield ()
  }

  private def getUpdatedPermissions()(using PlanContext): IO[Permissions] = {
    val sql =
      s"""
SELECT allow_teacher_manage, allow_student_select, allow_student_drop, allow_student_evaluate
FROM ${schemaName}.semester_phase_table
LIMIT 1;
       """.stripMargin

    for {
      _ <- IO(logger.info("读取更新后的权限信息"))
      json <- readDBJson(sql, List())
      updatedPermissions = Permissions(
        allowTeacherManage = decodeField[Boolean](json, "allow_teacher_manage"),
        allowStudentSelect = decodeField[Boolean](json, "allow_student_select"),
        allowStudentDrop = decodeField[Boolean](json, "allow_student_drop"),
        allowStudentEvaluate = decodeField[Boolean](json, "allow_student_evaluate")
      )
      _ <- IO(logger.info(s"更新后的权限信息: ${updatedPermissions}"))
    } yield updatedPermissions
  }
}