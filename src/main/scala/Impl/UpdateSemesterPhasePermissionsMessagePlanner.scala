package Impl

import Utils.SemesterPhaseProcess.{validateAdminToken, recordAdminOperationLog}
import Objects.SemesterPhaseService.Permissions
import Common.API.{PlanContext, Planner}
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.generic.auto._
import cats.implicits.*
import org.joda.time.DateTime
import Common.DBAPI._
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

  override def plan(using PlanContext): IO[Permissions] = {
    for {
      _ <- IO(logger.info("开始执行 UpdateSemesterPhasePermissions API"))

      // Step 1: 验证 adminToken 是否有效
      isAdminTokenValid <- validateAdminToken(adminToken)
      _ <- if (!isAdminTokenValid) IO.raiseError(new IllegalArgumentException("管理员Token无效")) 
           else IO(logger.info(s"管理员Token验证通过: ${adminToken}"))

      // Step 2: 获取当前阶段
      currentPhase <- getCurrentPhase()
      _ <- IO(logger.info(s"当前学期阶段为: ${currentPhase}"))

      // Step 2.1 & 2.2: 验证权限在当前阶段是否允许开启
      _ <- validatePermissionByPhase(currentPhase)

      // Step 3: 更新 SemesterPhaseTable 中的权限字段
      _ <- updateSemesterPhaseTable()

      // Step 4: 封装更新后的权限字段为 Permissions 对象并返回
      updatedPermissions <- getUpdatedPermissions()
      _ <- IO(logger.info(s"更新后的权限为: ${updatedPermissions}"))

      // Step 5: 记录权限变更操作的日志
      _ <- recordPermissionsChangeLog(updatedPermissions)
    } yield updatedPermissions
  }

  private def getCurrentPhase()(using PlanContext): IO[Int] = {
    val sql = s"SELECT current_phase FROM ${schemaName}.semester_phase_table LIMIT 1;"
    readDBInt(sql, List.empty).flatTap(phase => IO(logger.info(s"从数据库获取到的当前阶段是: ${phase}")))
  }

  private def validatePermissionByPhase(currentPhase: Int)(using PlanContext): IO[Unit] = {
    val errorMessage = currentPhase match {
      case 1 if allowStudentEvaluate =>
        "试图开启当前阶段不允许的权限: allowStudentEvaluate 在阶段1不可开启"
      case 2 if allowTeacherManage =>
        "试图开启当前阶段不允许的权限: allowTeacherManage 在阶段2不可开启"
      case _ =>
        ""
    }

    if (errorMessage.nonEmpty) IO.raiseError(new IllegalArgumentException(errorMessage))
    else IO(logger.info("权限验证通过"))
  }

  private def updateSemesterPhaseTable()(using PlanContext): IO[Unit] = {
    val sql =
      s"""
         UPDATE ${schemaName}.semester_phase_table
         SET allow_teacher_manage = ?, 
             allow_student_select = ?, 
             allow_student_drop = ?, 
             allow_student_evaluate = ?;
         """.stripMargin
         
    val parameters = List(
      SqlParameter("Boolean", allowTeacherManage.toString),
      SqlParameter("Boolean", allowStudentSelect.toString),
      SqlParameter("Boolean", allowStudentDrop.toString),
      SqlParameter("Boolean", allowStudentEvaluate.toString)
    )
    
    writeDB(sql, parameters).flatMap { result =>
      IO(logger.info(s"数据库更新成功 - 返回信息: $result"))
    }.void
  }

  private def getUpdatedPermissions()(using PlanContext): IO[Permissions] = {
    val sql = s"SELECT * FROM ${schemaName}.semester_phase_table LIMIT 1;"
    readDBJson(sql, List.empty).map { json => 
      Permissions(
        allowTeacherManage = decodeField[Boolean](json, "allow_teacher_manage"),
        allowStudentSelect = decodeField[Boolean](json, "allow_student_select"),
        allowStudentDrop = decodeField[Boolean](json, "allow_student_drop"),
        allowStudentEvaluate = decodeField[Boolean](json, "allow_student_evaluate")
      )
    }
  }

  private def recordPermissionsChangeLog(updatedPermissions: Permissions)(using PlanContext): IO[Unit] = {
    val operation = "更新权限"
    val details = s"""老师管理=${updatedPermissions.allowTeacherManage}, 学生选课=${updatedPermissions.allowStudentSelect}, 学生退课=${updatedPermissions.allowStudentDrop}, 学生评价=${updatedPermissions.allowStudentEvaluate}""".stripMargin
    recordAdminOperationLog(operation, details).flatMap { result =>
      IO(logger.info(s"权限变更日志记录成功 - 返回信息: $result"))
    }.void
  }
}