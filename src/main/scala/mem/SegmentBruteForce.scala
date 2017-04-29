package mem

object SegmentBruteForce extends App {
  val repr = for {
    seg <- 0 to 0x10000
    off <- 0 to 0x10000
    if (seg * 16 + off) % 0x100000 == 0x7c00
  } yield (seg, off)
}
