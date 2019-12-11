package computer

trait ArgumentMode {
  def get(dataset: List[Int], value: Int): Int
}

case class PositionMode() extends ArgumentMode {
  override def get(dataset: List[Int], value: Int): Int = dataset(value)
}

case class ImmediateMode() extends ArgumentMode {
  override def get(dataset: List[Int], value: Int): Int = value
}