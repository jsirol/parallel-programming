
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    if (x < 0 || x >= src.width || y < 0 || y >= src.height)
      throw new IllegalArgumentException("x/y coordinate out of bounds")
    var channels = (0, 0, 0, 0)
    var npix = 0
    var xc = x - radius
    while (xc <= x + radius) {
      if (xc == clamp(xc, 0, src.width - 1)) {
        var yc = y - radius
        while (yc <= y + radius) {
          if (yc == clamp(yc, 0, src.height - 1)) {
            val pix = src.apply(xc, yc)
            channels = (channels._1 + red(pix),
              channels._2 + green(pix),
              channels._3 + blue(pix),
              channels._4 + alpha(pix))
            npix += 1
          }
          yc += 1
        }
      }
      xc += 1
    }
    rgba(channels._1 / npix, channels._2 / npix, channels._3 / npix, channels._4 / npix)

    // functional version
    /*    val channels = for {
      i <- x - radius to x + radius
      j <- y - radius to y + radius
      if (i == clamp(i, 0, src.width - 1) && j == clamp(j, 0, src.height - 1))
      pix = src.apply(i, j)
    } yield List(red(pix), green(pix), blue(pix), alpha(pix))
    val avgChannels = channels.reduce((c1, c2) => List(c1(0) + c2(0), c1(1) + c2(1), c1(2) + c2(2), c1(3) + c2(3))).map(e => e / channels.length)
    rgba(avgChannels(0), avgChannels(1), avgChannels(2), avgChannels(3))
  }*/
  }
}
