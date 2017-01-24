/**
  * Created by Neha on 23/01/2017.
  */

/**
  * Created by prashant on 23-01-2017.
  */

abstract class Students

case class StudentClass(id : Long , name : String,gender: String ) extends Students

case class Marks(subjectId : Int, studentId : Long, marksObtained : Float) extends Students

case class ScoreCard(studentId: Long, marks: Map[Long, Float], percentage: Float) extends Students

class Stud_Count(mList : List[Marks], sList : List[StudentClass]) {

  def marksMap(id: Long): Map[Long, Float] = {
    val value = for {
      k <- mList.filter(_.studentId == id).map(_.subjectId)
      l <- mList.filter(_.subjectId == k).filter(_.studentId == id).map(_.marksObtained)
      newvalue = (k.toLong, l)
    } yield newvalue
    value.toMap
  }


  def studentScoreCardMapping(): List[Map[String, ScoreCard]]= {
    val scoreCardList = for {
      id <- sList.map(_.id)
      name = sList.filter(_.id == id).map(c => c.name).head
      percent = mList.filter(_.studentId == id).map(_.marksObtained).foldRight(0.00)((a, b) => a + b)
      mm = marksMap(id.toLong)
      value = Map(name -> ScoreCard(id, mm, (percent / 5).toFloat))
    } yield value
    scoreCardList
  }

  def printScoreCard(name: String) : List[ScoreCard] = {
   val mappedList = studentScoreCardMapping()
    val scoreCardList = mappedList.filter(_.keys.head == name ).map(x => x(name)).sortBy(_.studentId)//Maintaining ascending order of Student Id
    scoreCardList match{                                                //Print only when the list is not empty.
      case x if (x.size > 0) => scoreCardList
      case _ => println("No data found")
              List()
    }
  }

  def getScoreCardByGender() :(List[List[ScoreCard]],List[List[ScoreCard]]) ={
    val (maleList,femaleList) = sList.partition(c => c.gender == "male")
    (maleList.map(_.name).map(x => printScoreCard(x)) ,femaleList.map(_.name).map(x => printScoreCard(x)))
  }

  def displayList(list : List[Any]) = list.foreach(x => println(x))

}



object Student extends App {

  object Gender extends Enumeration {
    val m ="male"
    val f = "female"
  }
  import Gender._

  val studentList = List(
    StudentClass(1, "Kapil",m),
    StudentClass(2, "Prashant",m),
    StudentClass(3, "Neha",f),
    StudentClass(4, "Preeti",f),
    StudentClass(5, "Shivangi",f),
    StudentClass(6, "Rohan",m),
    StudentClass(7, "Honey",m),
    StudentClass(8, "Vanshika",f),
    StudentClass(9, "Paakhi",f),
    StudentClass(10, "Divya",f))

  val marksList = List(
    Marks(1, 1, 96), Marks(2, 1, 97), Marks(3, 1, 68), Marks(4, 1, 44), Marks(5, 1, 15),
    Marks(1, 2, 43), Marks(2, 2, 95), Marks(3, 2, 98), Marks(4, 2, 58), Marks(5, 2, 45),
    Marks(1, 3, 67), Marks(2, 3, 92), Marks(3, 3, 95), Marks(4, 3, 88), Marks(5, 3, 89),
    Marks(1, 4, 79), Marks(2, 4, 33), Marks(3, 4, 89), Marks(4, 4, 23), Marks(5, 4, 13),
    Marks(1, 5, 27), Marks(2, 5, 55), Marks(3, 5, 97), Marks(4, 5, 65), Marks(5, 5, 78),
    Marks(1, 6, 66), Marks(2, 6, 26), Marks(3, 6, 46), Marks(4, 6, 69), Marks(5, 6, 46),
    Marks(1, 7, 90), Marks(2, 7, 26), Marks(3, 7, 38), Marks(4, 7, 68), Marks(5, 7, 93),
    Marks(1, 8, 54), Marks(2, 8, 73), Marks(3, 8, 29), Marks(4, 8, 79), Marks(5, 8, 83),
    Marks(1, 9, 78), Marks(2, 9, 91), Marks(3, 9, 80), Marks(4, 9, 19), Marks(5, 9, 64),
    Marks(1, 10, 47), Marks(2, 10, 49), Marks(3, 10, 97), Marks(4, 10, 87), Marks(5, 10, 65))

  val obj = new Stud_Count(marksList,studentList)

  println(studentList.filter(_.id == 2).map(c => c.gender))
  obj.getScoreCardByGender()

  obj.displayList(obj.studentScoreCardMapping())

  obj.printScoreCard("Neha").foreach(x => println(x))
  val (maleScoreCards,femaleScoreCards) = obj.getScoreCardByGender()
  println(obj.displayList(maleScoreCards))
  println(obj.displayList(femaleScoreCards))
}
