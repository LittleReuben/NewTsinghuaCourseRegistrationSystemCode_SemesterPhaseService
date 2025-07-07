package Impl

import Utils.SemesterPhaseProcess.validateAdminToken
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
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
import io.circe.syntax._
import io.circe.generic.auto._
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class RollBackToPhase1ServicePlanner(
                                           adminToken: String,
                                           override val planContext: PlanContext
                                         ) extends Planner[Boolean] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[Boolean] = {
    for {
      // Step 1: Verify the validity of the adminToken
      _ <- IO(logger.info("[Step 1] 开始验证adminToken的有效性"))
      isValid <- validateAdminToken(adminToken)
      _ <- if (!isValid) {
        IO.raiseError(new IllegalArgumentException("[Step 1.1] Token验证失败"))
      } else {
        IO(logger.info("[Step 1.2] Token验证成功"))
      }

      // Step 2: Clear all course-related tables
      _ <- IO(logger.info("[Step 2] 开始清空选课相关表格"))
      _ <- clearCourseTables()

      // Step 3: Update the current phase to phase 1 in SemesterPhaseTable
      _ <- IO(logger.info("[Step 3] 更新学期阶段内容为Phase1"))
      _ <- updateCurrentPhase()
    } yield true
  }

  private def clearCourseTables()(using PlanContext): IO[Unit] = {
    val tables = List(
      "course_preselection_table",
      "course_selection_table",
      "waiting_list_table",
      "course_participation_history_table",
      "course_evaluation_table"
    )

    tables.map { table =>
      val sql = s"DELETE FROM ${schemaName}.$table;"
      IO(logger.info(s"清空数据库表: $table")) >>
        writeDB(sql, List.empty).flatMap(response =>
          IO(logger.info(s"清空表 $table 的SQL执行结果: $response"))
        )
    }.sequence_.void
  }

  private def updateCurrentPhase()(using PlanContext): IO[Unit] = {
    val sql =
      s"""
         |UPDATE ${schemaName}.semester_phase_table
         |SET current_phase = ?,
         |    allow_teacher_manage = ?,
         |    allow_student_select = ?,
         |    allow_student_drop = ?,
         |    allow_student_evaluate = ?;
         |""".stripMargin

    val parameters = List(
      SqlParameter("Int", "1"), // Setting current_phase to 1
      SqlParameter("Boolean", false.toString), // Disallow teacher management
      SqlParameter("Boolean", false.toString), // Disallow student course selection
      SqlParameter("Boolean", false.toString), // Disallow student course drop
      SqlParameter("Boolean", false.toString)  // Disallow student course evaluation
    )

    writeDB(sql, parameters).flatMap { response =>
      IO(logger.info(s"更新学期阶段表的SQL执行结果: ${response}"))
    }
  }
}