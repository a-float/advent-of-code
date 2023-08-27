package advent
import scala.collection.mutable
import scala.io.Source

object Day14 extends Day[String] {
  override val day: Int = 14

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  override def part1(): String = {
    val recipes = parseInput(getSource)
    toRaw(Chemical("FUEL", 1), recipes).toString
  }
  override def part2(): String = {
    val recipes = parseInput(getSource)
    var min = (1e12 / toRaw(Chemical("FUEL", 1), recipes)).floor.toLong
    var max = 1e12.toLong
    while (true) {
      val mid = (min + max) / 2
      val midVal = toRaw(Chemical("FUEL", mid), recipes)
      if (midVal < 1e12) {
        min = mid
      } else {
        max = mid
      }
      if (max - min == 1 || max - mid == 1) {
        return mid.toString
      }
    }
    "unknown"
  }

  private def parseInput(src: Source): Map[String, Recipe] = src
    .getLines()
    .map("""(\d+) (\w+)""".r.findAllMatchIn(_))
    .map(r => r.map(m => Chemical(m.group(2), m.group(1).toInt)).toList.reverse)
    .map(x => x.head.name -> Recipe(x.head.quantity, x.drop(1)))
    .toMap

  private def toRaw(
      chemical: Chemical,
      recipes: Map[String, Recipe],
      storage: mutable.Map[String, Long] = mutable.Map.empty
  ): Long = {
    val recipe = recipes(chemical.name)
    val toMake = chemical.quantity - storage.getOrElse(chemical.name, 0L)
    if (toMake <= 0) {
      toMake match {
        case 0     => storage.remove(chemical.name)
        case other => storage.update(chemical.name, -other)
      }
      return 0
    }
    val times = (toMake + recipe.give - 1) / recipe.give
    val redundant = times * recipe.give - toMake
    if (redundant > 0) storage.update(chemical.name, redundant)
    if (recipe.need.size == 1 && recipe.need.head.name == "ORE") {
      times * recipe.need.head.quantity
    } else {
      recipe.need
        .map(c => toRaw(Chemical(c.name, c.quantity * times), recipes, storage))
        .sum
    }
  }

  private case class Chemical(name: String, quantity: Long)
  private case class Recipe(give: Long, need: List[Chemical])
}
