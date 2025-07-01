package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Objects.SemesterPhaseService.{Phase, Permissions, SemesterPhase}
import APIs.UserAuthService.VerifyTokenValidityMessage
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import Common.ServiceUtils.schemaName
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
import Objects.SemesterPhaseService.Phase
import Objects.SemesterPhaseService.SemesterPhase
import Objects.SemesterPhaseService.Permissions
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Objects.SemesterPhaseService.Permissions

case class QuerySemesterPhaseStatusMessagePlanner(
    userToken: String,
    override val planContext: PlanContext
) extends Planner[SemesterPhase] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[SemesterPhase] = {
    for {
      // Step 1: Verify Token Validity
      _ <- IO(logger.info(s"[Step 1] 验证用户Token: ${userToken} 开始"))
      isValid <- VerifyTokenValidityMessage(userToken).send
      _ <- if (!isValid) {
        IO(logger.error(s"[Step 1.1] Token验证失败，userToken: ${userToken}")) >> 
        IO.raiseError(new IllegalStateException("Token验证失败"))
      } else {
        IO(logger.info(s"[Step 1.1] Token验证通过，userToken: ${userToken}"))
      }

      // Step 2: Fetch Semester Phase Data from Database
      _ <- IO(logger.info(s"[Step 2] 开始从数据库查询学期阶段和相应操作权限"))
      (currentPhase, permissions) <- fetchSemesterPhaseData()

      // Step 3: Construct SemesterPhase Object
      _ <- IO(logger.info(s"[Step 3] 构造 SemesterPhase 对象，当前阶段为 ${currentPhase}"))
    } yield SemesterPhase(currentPhase, permissions)
  }

  private def fetchSemesterPhaseData()(using PlanContext): IO[(Phase, Permissions)] = {
    val sql =
      s"""
      SELECT current_phase, allow_teacher_manage, allow_student_select, allow_student_drop, allow_student_evaluate
      FROM ${schemaName}.semester_phase_table
      WHERE current_phase IS NOT NULL
      LIMIT 1;
      """
    IO(logger.info(s"[fetchSemesterPhaseData] 执行查询学期阶段的 SQL: ${sql}")) >>
    readDBJson(sql, List.empty).map { json =>
      val phaseInt = decodeField[Int](json, "current_phase")
      val currentPhase = Phase.fromString(phaseToString(phaseInt))
      val permissions = Permissions(
        allowTeacherManage = decodeField[Boolean](json, "allow_teacher_manage"),
        allowStudentSelect = decodeField[Boolean](json, "allow_student_select"),
        allowStudentDrop = decodeField[Boolean](json, "allow_student_drop"),
        allowStudentEvaluate = decodeField[Boolean](json, "allow_student_evaluate")
      )
      (currentPhase, permissions)
    }.handleErrorWith { err =>
      val errorMsg = s"[fetchSemesterPhaseData] 查询学期阶段数据时发生错误: ${err.getMessage}"
      IO(logger.error(errorMsg)) >> IO.raiseError(err)
    }
  }

  private def phaseToString(phaseInt: Int): String = {
    IO(logger.info(s"[phaseToString] 将阶段值 ${phaseInt} 映射为阶段名称")).unsafeRunSync()
    phaseInt match {
      case 1 => "Phase1"
      case 2 => "Phase2"
      case _ =>
        val errorMsg = s"未知的阶段值: ${phaseInt}"
        IO(logger.error(errorMsg)).unsafeRunSync()
        throw new IllegalStateException(errorMsg)
    }
  }
}