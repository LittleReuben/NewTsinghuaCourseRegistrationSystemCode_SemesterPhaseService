import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.joda.time.DateTime

def SemesterPhaseProcess()(using PlanContext): IO[Unit] = {
  val logger = LoggerFactory.getLogger("SemesterPhaseProcess")

  logger.info(s"[SemesterPhaseProcess] 开始进行学期阶段管理相关逻辑处理...")

  val fetchSemesterPhaseSQL = 
    s"""
    SELECT phase_id, phase_name, start_time, end_time, is_active
    FROM ${schemaName}.semester_phase
    WHERE CURRENT_TIMESTAMP BETWEEN start_time AND end_time;
    """

  logger.info(s"[SemesterPhaseProcess] 准备执行查询学期阶段的SQL: ${fetchSemesterPhaseSQL}")

  for {
    fetchedPhaseData <- readDBRows(fetchSemesterPhaseSQL, Nil) // 无参数查询
    _ <- IO(logger.info(s"[SemesterPhaseProcess] 已查询到学期阶段信息，共 ${fetchedPhaseData.size} 条记录"))

    phaseInfo <- IO {
      fetchedPhaseData.map { json =>
        val phaseID = decodeField[Int](json, "phase_id")
        val phaseName = decodeField[String](json, "phase_name")
        val startTime = decodeField[DateTime](json, "start_time")
        val endTime = decodeField[DateTime](json, "end_time")
        val isActive = decodeField[Boolean](json, "is_active")

        logger.info(s"处理学期阶段信息: ID=${phaseID}, 名称=${phaseName}, 开始时间=${startTime}, 结束时间=${endTime}, 是否激活=${isActive}")
        (phaseID, phaseName, startTime, endTime, isActive)
      }
    }

    _ <- if (phaseInfo.nonEmpty) {
      IO {
        phaseInfo.foreach { case (phaseID, phaseName, startTime, endTime, isActive) =>
          logger.info(s"学期阶段[${phaseName}] (ID: ${phaseID}) 在时间 ${startTime} 至 ${endTime} 内是激活状态: ${isActive}")
        }
      }
    } else {
      IO(logger.info("[SemesterPhaseProcess] 当前无学期阶段符合时间范围条件。"))
    }

  } yield ()
}