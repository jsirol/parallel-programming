package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

  import FloatOps._
  import simulatorTest._


  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  test("Fork.insert(b) should insert recursively in the appropriate quadrant") {
    val quad = Fork(Empty(2.5f, 2.5f, 5f), Empty(7.5f, 2.5f, 5f), Empty(2.5f, 7.5f, 5f), Empty(7.5f, 7.5f, 5f))
    val b = new Body(3f, 1f, 1f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted.nw match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 2.5f, s"$centerX should be 2.5f")
        assert(centerY == 2.5f, s"$centerY should be 2.5f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Fork.insert() should have returned a Leaf as nw, was $inserted")
    }
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }



  // own tests for leaf, fork
  test("leaf returns new fork if size > minimumsize") {
    val fork = new Fork(
      new Empty(2.5f, 2.5f, 5.0f),
      new Empty(7.5f, 2.5f, 5.0f),
      new Empty(2.5f, 7.5f, 5.0f),
      new Empty(7.5f, 7.5f, 5.0f))

    val newfork = fork.insert(new Body(2, 1, 2, 1, 1))
    newfork.nw match {
      case Fork(nwsub, nesub, swsub, sesub) => assert(false, "fail: fork instead of leaf")
      case Empty(x, y, size) => assert(false, "fail: empty instead of leaf")
      case Leaf(x, y, size, bodies) => assert(true, "success: leaf found")
    }
  }



  // testing simulator
  test("updateBoundaries") {
    val body = new Body(5, 0, 105, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val newBoundaries = sim.updateBoundaries(boundaries, body)
    assert(newBoundaries.minX === 0, "minX updates right")
    assert(newBoundaries.maxY === 105, "maxY updates right")
  }

  test("mergeBoundaries") {
    val boundaries1 = new Boundaries()
    boundaries1.minX = 1
    boundaries1.minY = 1
    boundaries1.maxX = 97
    boundaries1.maxY = 97
    val boundaries2 = new Boundaries()
    boundaries2.minX = 45
    boundaries2.minY = 10
    boundaries2.maxX = 12
    boundaries2.maxY = 100
    val mergedBoundaries = sim.mergeBoundaries(boundaries1, boundaries2)
    assert(mergedBoundaries.minX === 1, "minX marged right")
    assert(mergedBoundaries.minY === 1, "minY merged right")
    assert(mergedBoundaries.maxX === 97, "maxX merged right")
    assert(mergedBoundaries.maxY === 100, "maxY merged right")
  }

  test("computeSectorMatrix sectorSize right") {
    val bodies = Seq(new Body(5, 0, 105, 0.1f, 0.1f))
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = sim.computeSectorMatrix(bodies, boundaries)
    assert(sm.sectorSize === 96 / SECTOR_PRECISION)
  }


}

object simulatorTest {
  val simModel = new SimulationModel()
  val sim = new Simulator(simModel.taskSupport, simModel.timeStats)
}



object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

