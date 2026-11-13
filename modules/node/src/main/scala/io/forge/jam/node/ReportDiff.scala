package io.forge.jam.node

/** The fields in which two values (work reports, typically) differ, by path, e.g.
  * `packageSpec.erasureRoot: ours=… theirs=…`. Case classes and sequences are compared field by field and element by
  * element; anything else by equality. For diagnosing a peer that computed a different report.
  */
object ReportDiff:

  def diff(ours: Any, theirs: Any, path: String = ""): List[String] =
    (ours, theirs) match
      case (a: Product, b: Product)
          if a.getClass == b.getClass && a.productArity > 0 && !a.isInstanceOf[Iterable[?]] =>
        a.productElementNames.zip(a.productIterator.zip(b.productIterator)).toList.flatMap { case (name, (x, y)) =>
          diff(x, y, if path.isEmpty then name else s"$path.$name")
        }
      case (a: Iterable[?], b: Iterable[?]) =>
        val (as, bs) = (a.toList, b.toList)
        if as.size != bs.size then List(s"$path: ${as.size} entries ours, ${bs.size} theirs")
        else as.zip(bs).zipWithIndex.flatMap { case ((x, y), i) => diff(x, y, s"$path[$i]") }
      case (a: Array[?], b: Array[?]) =>
        if java.util.Objects.deepEquals(a, b) then Nil else List(s"$path: ours=${show(a)} theirs=${show(b)}")
      case (a, b) => if a == b then Nil else List(s"$path: ours=${show(a)} theirs=${show(b)}")

  private def show(x: Any): String =
    val s = x match
      case a: Array[Byte] => a.map(b => f"$b%02x").mkString
      case other          => String.valueOf(other)
    if s.length > 140 then s.take(140) + "…" else s
