import ParallelMechanism.parallel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future}

object HttpRequest {

  // get request connection
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  def get(url: String, connectTimeout: Int = 5000, readTimeout: Int = 5000, requestMethod: String = "GET"): String = {
    import java.net.{URL, HttpURLConnection}
    val connection = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(connectTimeout)
    connection.setReadTimeout(readTimeout)
    connection.setRequestMethod(requestMethod)

    val inputStream = connection.getInputStream
    val content = io.Source.fromInputStream(inputStream).mkString
    if (inputStream != null) inputStream.close()
    content
  }

  def getData(url1: String, url2: String): (String, String) = {
    try {
      (url1, url2) match {
        case(url1, "") =>
          val content1 = get(url1)
          val content2 = get(url1)
          (content1, content2)
        case _ =>
          val content1 = get(url1)
          val content2 = get(url2)
          (content1, content2)
      }
    } catch {
      case ioe: java.io.IOException => throw new Exception(ioe)
      case ste: java.net.SocketTimeoutException => throw new Exception(ste)
    }
  }

  def getDataParallel(url1: String, url2: String): (String, String) = {
    try {
      (url1, url2) match {
        case (url1, "") => parallel(get(url1), get(url1))
        case (url1, url2) => parallel(get(url1), get(url2))
      }
    } catch {
      case ioe: java.io.IOException => throw new Exception(ioe)
      case ste: java.net.SocketTimeoutException => throw new Exception(ste)
    }
  }

  def getDataFuture(url1: String, url2: String): (String, String) = {
    try {
      (url1, url2) match {
        case (url1, "") =>
          val callToBeResolved1 = Future(get(url1))
          val callToBeResolved2 = Future(get(url1))
          (Await.result(callToBeResolved1, 2000.second), Await.result(callToBeResolved2, 2000.second))
        case (url1, url2) =>
          val callToBeResolved1 = Future(get(url1))
          val callToBeResolved2 = Future(get(url2))

          (Await.result(callToBeResolved1, 2000.second), Await.result(callToBeResolved2, 2000.second))
      }
    } catch {
      case ioe: java.io.IOException => throw new Exception(ioe)
      case ste: java.net.SocketTimeoutException => throw new Exception(ste)
    }
  }
}

