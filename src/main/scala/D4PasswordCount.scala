object D4PasswordCount {

  def main(args: Array[String]): Unit = {
    val range = args(0).split("-")

    val lower = range(0).toInt
    val upper = range(1).toInt

    val possibleItems = Stream
      .range(lower, upper)
      .count(item => {
      val string = item.toString
      string.length <= 6 &&
        string.groupBy(char => char).exists(_._2.length == 2) &&
        string.sorted.mkString("") == string
    })


    println(possibleItems)

  }
}
