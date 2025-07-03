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
import Objects.UserRole.UserRole

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
  // val logger = LoggerFactory.getLogger("AdminTokenValidation")  // 同文后端处理: logger 统一
    logger.info(s"开始验证管理员 Token: ${adminToken}")
  
    val sqlUserIdLookup = s"SELECT user_id FROM ${schemaName}.user_token_table WHERE token = ?"
    val sqlRoleCheck = s"SELECT role FROM ${schemaName}.user_account_table WHERE user_id = ?"
  
    for {
      _ <- IO(logger.info(s"在 user_token_table 中查找 token 是否存在，SQL: ${sqlUserIdLookup}"))
      optionalUserJson <- readDBJsonOptional(sqlUserIdLookup, List(SqlParameter("String", adminToken)))
  
      // 如果未找到对应的用户记录，直接返回 false
      isValid <- optionalUserJson match {
        case None =>
          logger.info(s"Token ${adminToken} 未找到对应的用户记录，验证失败")
          IO(false)
  
        case Some(userJson) =>
          val userID = decodeField[Int](userJson, "user_id")
          logger.info(s"Token ${adminToken} 对应的 user_id 为: ${userID}")
  
          logger.info(s"在 user_account_table 中查找用户的角色，SQL: ${sqlRoleCheck}")
          for {
            optionalRoleJson <- readDBJsonOptional(sqlRoleCheck, List(SqlParameter("Int", userID.toString)))
  
            // 如果未找到对应角色记录，返回 false；否则判断角色是否为超级管理员
            result <- optionalRoleJson match {
              case None =>
                logger.info(s"user_id ${userID} 未找到角色记录，验证失败")
                IO(false)
  
              case Some(roleJson) =>
                val role = UserRole.fromString(decodeField[String](roleJson, "role"))
                logger.info(s"user_id ${userID} 的角色为: ${role.toString}")
                IO(role == UserRole.SuperAdmin)
            }
          } yield result
      }
    } yield isValid
  }
  // 修复内容:
  // 1. 将 user_id 的数据类型从 String 更改为 Int，并使用 decodeField[Int] 提取数据。
  // 2. 使用 UserRole.fromString 方法转换 decodeField 提取后的 role 为枚举类型，并将其与 UserRole.SuperAdmin 比较。
}
