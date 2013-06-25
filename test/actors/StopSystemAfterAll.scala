package actors

import org.scalatest.{ Suite, BeforeAndAfterAll }
import akka.testkit.TestKit
import akka.actor.ActorSystem
import akka.testkit.ImplicitSender
import org.specs2.specification.After
import org.specs2.time.NoTimeConversions
import org.specs2.specification.Scope

trait StopSystemAfterAll extends BeforeAndAfterAll {

  this: TestKit with Suite =>

  override protected def afterAll() {
    super.afterAll()
    system.shutdown()
  }

}
