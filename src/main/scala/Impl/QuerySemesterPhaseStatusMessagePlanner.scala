package Impl

import Objects.SemesterPhaseService.{Phase, SemesterPhase, Permissions}
import APIs.UserAuthService.VerifyTokenValidityMessage
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe.Json
import org.joda.time.DateTime
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}

case class QuerySemesterPhaseStatusMessagePlanner(
                                                   userToken: String,
                                                   override val planContext: PlanContext
                                                 ) extends Planner[SemesterPhase] {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[SemesterPhase] = {
    for {
      // Step 1: Verify token validity
      _ <- IO(logger.info(s"开始验证用户token: ${userToken} 的合法性"))
      isValid <- verifyToken()
      _ <- IO(logger.info(s"Token验证结果为: ${isValid}"))
      _ <- if (!isValid) IO.raiseError(new IllegalStateException("Token验证失败")) else IO.unit

      // Step 2: Retrieve current phase and permissions from the database
      _ <- IO(logger.info("从数据库查询学期阶段信息和权限配置"))
      currentPhase <- getCurrentPhase()
      permissions <- getPermissions()

      // Step 3: Wrap into SemesterPhase object
      _ <- IO(logger.info("封装为SemesterPhase对象"))
    } yield SemesterPhase(currentPhase = currentPhase, permissions = permissions)
  }

  /** Step 1: Verify the user token validity from API */
  private def verifyToken()(using PlanContext): IO[Boolean] = {
    IO(logger.info("调用VerifyTokenValidityMessage验证用户Token"))
      .>>(VerifyTokenValidityMessage(userToken).send)
  }

  /** Step 2.1: Get the current semester phase as Phase Enum from the database */
  private def getCurrentPhase()(using PlanContext): IO[Phase] = {
    val sql =
      s"""
      SELECT current_phase
      FROM ${schemaName}.semester_phase_table
      LIMIT 1
      """.stripMargin

    for {
      _ <- IO(logger.info(s"执行获取current_phase的SQL: $sql"))
      phaseInt <- readDBInt(sql, List())
      _ <- IO(logger.info(s"查询结果：当前阶段为整数值: $phaseInt"))
      phase <- IO(Phase.fromString(s"phase$phaseInt")) // Fix: Changed Phase.fromString implementation according to the enumeration's expected structure.
    } yield phase
  }

  /** Step 2.2: Get permissions from the database */
  private def getPermissions()(using PlanContext): IO[Permissions] = {
    val sql =
      s"""
      SELECT allow_teacher_manage, allow_student_select, allow_student_drop, allow_student_evaluate
      FROM ${schemaName}.semester_phase_table
      LIMIT 1
      """.stripMargin

    for {
      _ <- IO(logger.info(s"执行获取权限配置的SQL: $sql"))
      json <- readDBJson(sql, List())
      allowTeacherManage <- IO { decodeField[Boolean](json, "allow_teacher_manage") }
      allowStudentSelect <- IO { decodeField[Boolean](json, "allow_student_select") }
      allowStudentDrop <- IO { decodeField[Boolean](json, "allow_student_drop") }
      allowStudentEvaluate <- IO { decodeField[Boolean](json, "allow_student_evaluate") }
      _ <- IO(logger.info(
        s"权限配置：教师管理权限=$allowTeacherManage, 学生选课权限=$allowStudentSelect, 学生退课权限=$allowStudentDrop, 学生评价权限=$allowStudentEvaluate"
      ))
    } yield Permissions(
      allowTeacherManage = allowTeacherManage,
      allowStudentSelect = allowStudentSelect,
      allowStudentDrop = allowStudentDrop,
      allowStudentEvaluate = allowStudentEvaluate
    )
  }
}
// 编译错误修复原因:
// 修复了Phase的枚举方法调用逻辑，该错误是由于原始代码中尝试使用toRight方法，该方法在枚举类的Phase中无效。转而直接使用Phase.fromString方法以正确处理字符串到枚举对象的转换。