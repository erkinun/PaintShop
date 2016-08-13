import com.asyaminor.challenge.PaintShopV2.Bucket
import com.asyaminor.challenge._
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
val customerList = List("1 M 3 G 5 G", "2 G 3 M 4 G", "5 M")
val preferenceList = customerList map
  {customerString => turnIntoTupleList(customerString.split(" ").toList)}
preferenceList sortBy(pref => pref.size)
val likes = List.fill(10)(0)
likes(0)
likes.updated(0,1)
val choices: Map[String, List[Int]] = Map()
val newChoices = choices + ("2M" -> List(1))
newChoices("2M")


type Likes = Int

def increaseLikes(likes: List[Likes], index: Int): List[Likes] = {
  val current = likes(index)
  likes.updated(index, current + 1)
}
def decreaseLikes(likes: List[Likes], index: Int): List[Likes] = {
  val current = likes(index)
  println(current)
  if (current < 1) {
    //throw new NoSolutionCanBeFound()
  }

  likes.updated(index, current - 1)
}

increaseLikes(likes, 0)
val newLikes= decreaseLikes(likes, 0)
implicit class BucketPrint(bucket: Bucket) {
  def printBucket = bucket.foldLeft("")((str, paint) => paint.typePaint.toString + " " + str)
}
val bucket = List(ProducedPaint(Matte()), ProducedPaint(Matte()))

Matte().toString

println("printme")
println(bucket.printBucket)

val newBucket = bucket.updated(0, ProducedPaint(Gloss()))