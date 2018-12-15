import scala.util.Random

object SecretSanta extends App {

  val random = new Random
  val people = io.Source.fromFile("../SecretSanta/src/peoples.txt").getLines.toList

  def doRaffle(l: List[String]): List[String] = {
    def doRaffleHelper(helper_l: List[String], acc: List[String], peopleNames: List[String]): List[String] ={
      if (helper_l.isEmpty) peopleNames
      else {
        val nameSorted = getRandomElement(people.filter(!_.equals(helper_l.head)).filterNot(acc.contains(_)), random)
        doRaffleHelper(helper_l.tail, acc:+nameSorted, peopleNames:+getSecretSanta(helper_l.head, nameSorted))
      }
    }
    doRaffleHelper(l, Nil, Nil)
  }


  def getRandomElement(list: Seq[String], random: Random): String =
    list(random.nextInt(list.length))

  def getSecretSanta(people: String, sortedName: String): String = {
    people + " gets " + sortedName
  }

  doRaffle(scala.util.Random.shuffle(people)).foreach(println)
}
