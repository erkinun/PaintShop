import com.asyaminor.challenge.{Gloss, PaintType, Matte, Paint}

val cstStr = "1 M 3 G 5 G"

val input = cstStr.split(" ").toList


def turnIntoTupleList(input: List[String]): List[(Int, Char)] = {

  def turnInner(input: List[String], tuples: List[(Int, Char)]): List[(Int, Char)] = {
    input match {
      case i :: c :: rest =>
        val number = i.toInt
        val typeOfColor = c(0)
        turnInner(rest, (number, typeOfColor) :: tuples)
      case _ => tuples
    }
  }

  turnInner(input, List()).reverse
}

turnIntoTupleList(input)

def getPaintType(colorType: Char): PaintType = colorType match {
  case 'M' => Matte()
  case 'G' => Gloss()
  case _ => throw new IllegalArgumentException
}

getPaintType('G')

