package sttp.apispec.openapi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.{AnyFunSuite, AnyFunSuiteLike}
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ListMap

class OpenAPIComparatorTest extends AnyFlatSpec with Matchers {

  it should "detect missing and unxpected paths" in {
    OpenAPIComparator.comparePaths(
      ListMap("/hello/dragon" -> PathItem(), "/hello/fox" -> PathItem()),
      ListMap("/hello/dragon" -> PathItem(), "/hello/cat" -> PathItem())
    ) shouldBe List(
      PathIssue("/hello/cat", List(MissingPath)),
      PathIssue("/hello/fox", List(UnexpectedPath))
    )
  }

  it should "detect missing and unexpected operations" in {
    OpenAPIComparator.comparePaths(
      ListMap("/hello/dragon" -> PathItem(delete = Some(Operation()), patch = Some(Operation()))),
      ListMap(
        "/hello/dragon" -> PathItem(get = Some(Operation()), post = Some(Operation()), delete = Some(Operation()))
      )
    ) shouldBe List(
      PathIssue("/hello/dragon", List(MissingOperation("get"), MissingOperation("post"), UnexpectedOperation("patch")))
    )
  }
}
