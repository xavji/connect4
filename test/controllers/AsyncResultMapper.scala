package controllers

import scala.concurrent.Await
import scala.concurrent.duration._

import play.api.http.Writeable
import play.api.mvc.AsyncResult
import play.api.mvc.Result

import play.api.test.FakeRequest
import play.api.test.Helpers._

trait AsyncResultMapper {

  def routeAsync[T](req: FakeRequest[T])(implicit w: Writeable[T]): Result = {
    route(req) match {
      case Some(AsyncResult(future)) => Await.result(future, 3 seconds)
      case x => throw new RuntimeException("unexpected response " + x)
    }
  }

  def asyncResult(res: Result) = {
    res match {
      case AsyncResult(future) => Await.result(future, 3 seconds)
      case x => throw new RuntimeException("unexpected response " + x)
    }
  }
  
}