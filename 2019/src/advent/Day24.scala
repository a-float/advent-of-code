package advent

import scala.collection.mutable

object Day24 extends Day[Int] {
  override val day: Int = 24

  type Place = Set[Vector3]
  val W = 5
  val N = List(
    Vector3(0, 1, 0),
    Vector3(0, 0, 1),
    Vector3(0, -1, 0),
    Vector3(0, 0, -1)
  )

  val place: Place =
    getSource.getLines.zipWithIndex
      .flatMap((line, i) =>
        line.zipWithIndex
          .filter((c, j) => c != '.')
          .map((c, j) => Vector3(0, j, i))
      )
      .toSet

  def step(getNeighbours: (Vector3) => List[Vector3])(
      place: Place
  ) = {
    val nMap = place.toList
      .flatMap(getNeighbours)
      .groupBy(identity)
      .view
      .mapValues(_.size)

    nMap.keys
      .filter(p => {
        (place.contains(p), nMap.getOrElse(p, 0)) match {
          case (true, 1)  => true
          case (false, 1) => true
          case (false, 2) => true
          case _          => false
        }
      })
      .toSet
  }

  override def part1() = {
    val set = mutable.Set[Int]()

    def getNeighbours(p: Vector3) =
      N.map { p + _ }.filter { case Vector3(dim, x, y) =>
        (0 until W).contains(x) && (0 until W).contains(y)
      }

    def getBiodiversity(place: Place) = (0 until W)
      .flatMap(i => (0 until W).map(j => Vector3(0, j, i)))
      .filter(place.contains(_))
      .map(p => 1 << (p.z * W + p.y))
      .sum

    LazyList
      .iterate(place)(step(getNeighbours))
      .map(getBiodiversity)
      .dropWhile(bio => {
        if (set.contains(bio)) false
        else {
          set += bio
          true
        }
      })
      .take(1)
      .head
  }

  override def part2() = {
    def getNeighbours(p: Vector3) =
      N.flatMap(o => {
        (p + o) match {
          case Vector3(dim, -1, _) => Vector3(dim - 1, 1, 2) :: Nil
          case Vector3(dim, _, -1) => Vector3(dim - 1, 2, 1) :: Nil
          case Vector3(dim, W, _)  => Vector3(dim - 1, 3, 2) :: Nil
          case Vector3(dim, _, W)  => Vector3(dim - 1, 2, 3) :: Nil
          case Vector3(dim, 2, 2) =>
            p match {
              case Vector3(_, 1, _) => (0 to 4).map { Vector3(dim + 1, 0, _) }
              case Vector3(_, 3, _) => (0 to 4).map { Vector3(dim + 1, 4, _) }
              case Vector3(_, _, 1) => (0 to 4).map { Vector3(dim + 1, _, 0) }
              case Vector3(_, _, 3) => (0 to 4).map { Vector3(dim + 1, _, 4) }
            }
          case other => other :: Nil
        }
      })

    LazyList
      .iterate(place)(step(getNeighbours))
      .drop(200)
      .take(1)
      .head
      .size
  }
}
