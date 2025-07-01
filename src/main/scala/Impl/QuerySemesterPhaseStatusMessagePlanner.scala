package Impl


import Objects.SemesterPhaseService.Phase
import Objects.SemesterPhaseService.SemesterPhase
import APIs.UserAuthService.VerifyTokenValidityMessage
import Objects.SemesterPhaseService.Permissions
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
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
import Objects.SemesterPhaseService.Permissions
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class QuerySemesterPhaseStatusMessagePlanner(
    userToken: String,
    override val planContext: PlanContext
) extends Planner[SemesterPhase] {
  private val logger = LoggerFactory.getLogger(
    this.getClass.getSimpleName + "_" + planContext.traceID.id
  )

  override def plan(using planContext: PlanContext): IO[SemesterPhase] = {
    for {
      // Step 1: Verify the userToken validity
      _ <- IO(logger.info(s"开始验证Token: $userToken"))
      isTokenValid <- validateToken(userToken)
      _ <- IO.raiseUnless(isTokenValid)(new IllegalArgumentException("Token验证失败"))

      // Step 2: Query current semester phase and permissions
      _ <- IO(logger.info("开始查询当前阶段和权限信息"))
      (currentPhase, permissions) <- querySemesterPhaseDetails

      // Step 3: Construct SemesterPhase object
      _ <- IO(logger.info("构造SemesterPhase对象"))
    } yield SemesterPhase(
      currentPhase = Phase.fromString(currentPhase.toString),
      permissions = permissions
    )
  }

  private def validateToken(token: String)(using PlanContext): IO[Boolean] = {
    IO(logger.info("调用VerifyTokenValidityMessage验证Token有效性"))
    VerifyTokenValidityMessage(token).send
  }

  private def querySemesterPhaseDetails(using PlanContext): IO[(Int, Permissions)] = {
    val sql =
      s"""
         SELECT current_phase, allow_teacher_manage, allow_student_select, 
                allow_student_drop, allow_student_evaluate 
         FROM ${schemaName}.semester_phase_table LIMIT 1;
       """
    logger.info(s"查询SQL语句: $sql")
    readDBJson(sql, List()).map { json =>
      val currentPhase = decodeField[Int](json, "current_phase")
      val allowTeacherManage = decodeField[Boolean](json, "allow_teacher_manage")
      val allowStudentSelect = decodeField[Boolean](json, "allow_student_select")
      val allowStudentDrop = decodeField[Boolean](json, "allow_student_drop")
      val allowStudentEvaluate = decodeField[Boolean](json, "allow_student_evaluate")

      val permissions = Permissions(
        allowTeacherManage = allowTeacherManage,
        allowStudentSelect = allowStudentSelect,
        allowStudentDrop = allowStudentDrop,
        allowStudentEvaluate = allowStudentEvaluate
      )
      (currentPhase, permissions)
    }
  }
}