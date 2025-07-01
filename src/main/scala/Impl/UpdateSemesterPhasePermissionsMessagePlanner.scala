package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Objects.SemesterPhaseService.Permissions
import Utils.SemesterPhaseProcess.{validateAdminToken, recordAdminOperationLog}
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
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
import Objects.SystemLogService.SystemLogEntry
import Utils.SemesterPhaseProcess.recordAdminOperationLog
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

  override def plan(using PlanContext): IO[Permissions] = {
    for {
      // Step 1: Validate admin token
      _ <- IO(logger.info(s"验证管理员 token: ${adminToken}"))
      isAdminValid <- validateAdminToken(adminToken)
      _ <- if (!isAdminValid) IO.raiseError(new IllegalArgumentException("管理员 Token 无效")) else IO(logger.info("管理员 Token 验证成功"))

      // Step 2: Check if permissions are allowed in the current phase
      _ <- IO(logger.info("检查当前阶段是否允许设置传入的权限"))
      currentPhase <- getCurrentSemesterPhase
      _ <- IO(logger.info(s"当前阶段为: ${currentPhase}"))
      _ <- checkPhaseRestrictions(currentPhase, allowTeacherManage, allowStudentEvaluate)

      // Step 3: Update permissions in the SemesterPhaseTable
      _ <- IO(logger.info("更新 SemesterPhaseTable 中的权限字段"))
      _ <- updatePermissionsInDB(allowTeacherManage, allowStudentSelect, allowStudentDrop, allowStudentEvaluate)

      // Step 4: Encapsulate the updated permissions into a Permissions object and return
      updatedPermissions <- getUpdatedPermissions
      _ <- IO(logger.info(s"返回更新后的权限设置: ${updatedPermissions}"))

      // Step 5: Record admin operation log
      _ <- IO(logger.info("记录管理员操作日志"))
      logDetails = s"Updated permissions to: ${updatedPermissions}"
      _ <- recordAdminOperationLog("UpdatePermissions", logDetails)
    } yield updatedPermissions
  }

  private def getCurrentSemesterPhase(using PlanContext): IO[Int] = {
    val sql =
      s"""
         SELECT current_phase
         FROM ${schemaName}.semester_phase_table
         LIMIT 1;
       """
    readDBInt(sql, List.empty)
  }

  private def checkPhaseRestrictions(
      currentPhase: Int,
      allowTeacherManage: Boolean,
      allowStudentEvaluate: Boolean
  )(using PlanContext): IO[Unit] = {
    for {
      _ <- if (currentPhase == 1 && allowStudentEvaluate) {
        IO.raiseError(new IllegalArgumentException("试图开启当前阶段不允许的权限: allowStudentEvaluate"))
      } else if (currentPhase == 2 && allowTeacherManage) {
        IO.raiseError(new IllegalArgumentException("试图开启当前阶段不允许的权限: allowTeacherManage"))
      } else {
        IO(logger.info(s"权限设置符合当前阶段 ${currentPhase} 的规则"))
      }
    } yield ()
  }

  private def updatePermissionsInDB(
      allowTeacherManage: Boolean,
      allowStudentSelect: Boolean,
      allowStudentDrop: Boolean,
      allowStudentEvaluate: Boolean
  )(using PlanContext): IO[String] = {
    val sql =
      s"""
         UPDATE ${schemaName}.semester_phase_table
         SET allow_teacher_manage = ?, 
             allow_student_select = ?, 
             allow_student_drop = ?, 
             allow_student_evaluate = ?
         WHERE current_phase = (SELECT current_phase FROM ${schemaName}.semester_phase_table LIMIT 1);
       """
    val parameters = List(
      SqlParameter("Boolean", allowTeacherManage.asJson.noSpaces),
      SqlParameter("Boolean", allowStudentSelect.asJson.noSpaces),
      SqlParameter("Boolean", allowStudentDrop.asJson.noSpaces),
      SqlParameter("Boolean", allowStudentEvaluate.asJson.noSpaces)
    )
    writeDB(sql, parameters)
  }

  private def getUpdatedPermissions(using PlanContext): IO[Permissions] = {
    val sql =
      s"""
         SELECT allow_teacher_manage, allow_student_select, allow_student_drop, allow_student_evaluate
         FROM ${schemaName}.semester_phase_table
         LIMIT 1;
       """
    readDBJson(sql, List.empty).map { json =>
      Permissions(
        allowTeacherManage = decodeField[Boolean](json, "allow_teacher_manage"),
        allowStudentSelect = decodeField[Boolean](json, "allow_student_select"),
        allowStudentDrop = decodeField[Boolean](json, "allow_student_drop"),
        allowStudentEvaluate = decodeField[Boolean](json, "allow_student_evaluate")
      )
    }
  }
}