package Process

import Common.API.{API, PlanContext, TraceID}
import Common.DBAPI.{initSchema, writeDB, readDBInt}
import Common.ServiceUtils.schemaName
import Global.ServerConfig
import cats.effect.IO
import io.circe.generic.auto.*
import java.util.UUID
import Global.DBConfig
import Process.ProcessUtils.server2DB
import Global.GlobalVariables

object Init {
  def init(config: ServerConfig): IO[Unit] = {
    given PlanContext = PlanContext(traceID = TraceID(UUID.randomUUID().toString), 0)
    given DBConfig = server2DB(config)

    val program: IO[Unit] = for {
      _ <- IO(GlobalVariables.isTest = config.isTest)
      _ <- API.init(config.maximumClientConnection)
      _ <- Common.DBAPI.SwitchDataSourceMessage(projectName = Global.ServiceCenter.projectName).send
      _ <- initSchema(schemaName)

      /** 学期阶段表，包含学期当前阶段及相关权限
       * current_phase: 学期当前阶段（1代表阶段1，2代表阶段2）
       * allow_teacher_manage: 是否允许教师创建、删除、修改课程组和课程
       * allow_student_select: 是否允许学生选课
       * allow_student_drop: 是否允许学生退课
       * allow_student_evaluate: 是否允许学生评价课程
       */
      _ <- writeDB(
        s"""
        CREATE TABLE IF NOT EXISTS "${schemaName}"."semester_phase_table" (
            current_phase SERIAL NOT NULL PRIMARY KEY,
            allow_teacher_manage BOOLEAN NOT NULL DEFAULT false,
            allow_student_select BOOLEAN NOT NULL DEFAULT false,
            allow_student_drop BOOLEAN NOT NULL DEFAULT false,
            allow_student_evaluate BOOLEAN NOT NULL DEFAULT false
        );
        """,
        List()
      )

      // 检查表是否已有条目
      count <- readDBInt(
        s"""SELECT COUNT(*) FROM "${schemaName}"."semester_phase_table";""",
        List()
      )

      // 如果为空，则插入默认条目
      _ <- if (count == 0)
        writeDB(
          s"""
          INSERT INTO "${schemaName}"."semester_phase_table"
          (current_phase, allow_teacher_manage, allow_student_select, allow_student_drop, allow_student_evaluate)
          VALUES (1, false, false, false, false);
          """,
          List()
        )
      else IO.unit
    } yield ()

    program.handleErrorWith(err => IO {
      println("[Error] Process.Init.init 失败, 请检查 db-manager 是否启动及端口问题")
      err.printStackTrace()
    })
  }
}