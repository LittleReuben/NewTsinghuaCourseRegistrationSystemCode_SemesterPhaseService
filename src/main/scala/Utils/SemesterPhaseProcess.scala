package Utils

//process plan import 预留标志位，不要删除
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import Common.DBAPI._
import Common.ServiceUtils.schemaName
import org.slf4j.LoggerFactory
import Objects.SystemLogService.SystemLogEntry
import Common.API.PlanContext
import Common.Object.SqlParameter
import cats.effect.IO
import cats.implicits._
import Common.API.{PlanContext, Planner}
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case object SemesterPhaseProcess {
  private val logger = LoggerFactory.getLogger(getClass)
  //process plan code 预留标志位，不要删除
  
  def recordAdminOperationLog(operation: String, details: String)(using PlanContext): IO[String] = {
  // val logger = LoggerFactory.getLogger("recordAdminOperationLog")  // 同文后端处理: logger 统一
    logger.info(s"开始记录超级管理员操作日志...")
  
    // Step 1: 验证输入参数是否有效
    if (operation.trim.isEmpty) {
      IO.raiseError(new IllegalArgumentException("operation参数不能为空"))
    } else if (details.trim.isEmpty) {
      IO.raiseError(new IllegalArgumentException("details参数不能为空"))
    } else {
      logger.info(s"输入参数验证通过，参数operation: ${operation}, 参数details: ${details}")
  
      // Step 2: 处理日志记录
      val timestamp = DateTime.now
      val userID = -1 // 超级管理员特定的userID，此处用-1作为示例
  
      logger.info(s"构造日志条目，时间戳为${timestamp}，操作类型为${operation}，详细信息为${details}")
  
      val systemLogEntry = SystemLogEntry(
        logID = 0, // logID 的生成通常交由数据库或主键生成器，这里设为占位
        timestamp = timestamp,
        userID = userID,
        action = operation,
        details = details
      )
      val insertSQL =
        s"""
         INSERT INTO ${schemaName}.system_log_table (timestamp, user_id, action, details)
         VALUES (?, ?, ?, ?)
        """
      val insertParams = List(
        SqlParameter("DateTime", timestamp.getMillis.toString),
        SqlParameter("Int", userID.toString),
        SqlParameter("String", operation),
        SqlParameter("String", details)
      )
  
      for {
        _ <- IO(logger.info(s"准备写入系统日志内容到数据库，SQL为：${insertSQL}"))
        resultMessage <- writeDB(insertSQL, insertParams)
        _ <- IO(logger.info(s"记录操作日志成功，数据库返回结果: ${resultMessage}"))
      } yield {
        logger.info(s"记录日志操作成功，返回结果消息：${resultMessage}")
        resultMessage
      }
    }
  }
  
  def validateAdminToken(adminToken: String)(using PlanContext): IO[Boolean] = {
  // val logger = LoggerFactory.getLogger(this.getClass)  // 同文后端处理: logger 统一
  
    // Step 1: Log the start of the function
    IO(logger.info(s"开始验证管理员 Token: ${adminToken}")).flatMap { _ =>
  
      // Step 2: Look up the admin role using their token
      readDBJsonOptional(
        s"""
         SELECT user_role 
         FROM ${schemaName}.admin_table 
         WHERE admin_token = ?;
        """.stripMargin,
        List(SqlParameter("String", adminToken))
      ).flatMap {
        case None =>
          // Token does not exist in the database
          IO(logger.error(s"管理员 Token 无效: ${adminToken}")) >>
          IO(false)
        case Some(adminJson) =>
          // Token exists, get the user role
          IO {
            val userRole = decodeField[String](adminJson, "user_role")
            logger.info(s"通过管理员 Token 获取用户角色: ${userRole}")
            userRole
          }.flatMap { userRole =>
            if (userRole != "SuperAdmin") {
              // Role is not SuperAdmin
              IO(logger.error(s"用户角色不是超级管理员: ${userRole}")) >>
              IO(false)
            } else {
              // Step 3: Verify permission based on the semester's current phase
              readDBBoolean(
                s"""
                 SELECT allow_teacher_manage
                 FROM ${schemaName}.semester_phase_table
                 WHERE current_phase = ?;
                """.stripMargin,
                List(SqlParameter("Int", "1")) // Assuming you are checking for "current_phase = 1"
              ).flatMap { allowTeacherManage =>
                if (!allowTeacherManage) {
                  // Permission is not granted
                  IO(logger.error("当前学期阶段不允许管理")) >>
                  IO(false)
                } else {
                  // Token is valid, role is SuperAdmin, and permission is granted
                  IO(logger.info("管理员 Token 验证通过，且用户角色为超级管理员")) >>
                  IO(true)
                }
              }
            }
          }
      }
    }
  }
  
  // 模型修复的编译错误: `IO(logger.info(s"开始验证管理员 Token: ${adminToken}")) >>` 中的后缀运算符语法问题。Scala 3 中，使用>>需要明确表达为flatMap链式结构或者显式提供作用域。
}
