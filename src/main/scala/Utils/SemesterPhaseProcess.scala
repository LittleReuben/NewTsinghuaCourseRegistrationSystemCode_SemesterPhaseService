import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.Object.ParameterList
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime

def SemesterPhaseProcess()(using PlanContext): IO[Unit] = {
  logger.info(s"[SemesterPhaseProcess] 开始学期阶段管理逻辑处理")

  val sqlQuery =
    s"""
      SELECT phase_id, phase_name, start_date, end_date
      FROM ${schemaName}.semester_phase
      WHERE phase_status = 'Active'
    """
  logger.info(s"[SemesterPhaseProcess] 查询学期阶段信息的 SQL 语句为：${sqlQuery}")

  for {
    _ <- IO(logger.info("[Step 1] 执行查询 SQL，获取当前学期阶段信息"))
    rows <- readDBRows(sqlQuery, List.empty)

    phaseDetails <- IO {
      rows.map { json =>
        val phaseId = decodeField[Int](json, "phase_id")
        val phaseName = decodeField[String](json, "phase_name")
        val startDate = new DateTime(decodeField[Long](json, "start_date"))
        val endDate = new DateTime(decodeField[Long](json, "end_date"))
        logger.debug(s"[Step 1.1] 解析学期阶段: phaseId=${phaseId}, phaseName=${phaseName}, startDate=${startDate}, endDate=${endDate}")
        (phaseId, phaseName, startDate, endDate)
      }
    }

    currentTimestamp <- IO {
      val now = DateTime.now
      logger.info(s"[Step 2] 获取当前时间戳: ${now}")
      now
    }

    _ <- IO(logger.info("[Step 3] 根据阶段时间范围处理学期阶段状态"))
    phaseStatusUpdates <- IO {
      phaseDetails.map { case (phaseId, phaseName, startDate, endDate) =>
        val newStatus =
          if (currentTimestamp.isBefore(startDate)) "Upcoming"
          else if (currentTimestamp.isAfter(endDate)) "Expired"
          else "Ongoing"
        logger.debug(s"[Step 3.1] 阶段ID=${phaseId}, 阶段名称=${phaseName} 的新状态为: ${newStatus}")
        (phaseId, newStatus)
      }
    }

    _ <- {
      val updateSql =
        s"""
          UPDATE ${schemaName}.semester_phase
          SET phase_status = ?
          WHERE phase_id = ?
        """
      val updateParams = phaseStatusUpdates.map { case (phaseId, newStatus) =>
        List(
          SqlParameter("String", newStatus),
          SqlParameter("Int", phaseId.toString)
        )
      }

      IO(logger.info("[Step 4] 批量更新学期阶段状态")) *>
        writeDBList(updateSql, updateParams.map(ParameterList)).void
    }

    _ <- IO(logger.info("[SemesterPhaseProcess] 学期阶段逻辑处理完成"))
  } yield ()
}