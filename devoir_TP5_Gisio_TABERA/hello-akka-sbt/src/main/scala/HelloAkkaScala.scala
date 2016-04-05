import akka.actor.{ ActorRef, ActorSystem, Props, Actor, Inbox } // ActorSystem et ActorRef important
import scala.concurrent.duration._

// Definir des messages 3
case object Greet
case class WhoToGreet(who: String)  // qui dit c'est qui
case class Greeting(message: String)// qui contient un text message
case class Banner (message : String)
case class Chercher(mot: String)
case class ListeAnagrammes(mots: List[String])
case class Clavier(touches: String)
case class Nombre(n:Int)


class Greeter extends Actor {
  var greeting = ""     // var mutable

  // contient une methode
  def receive = {
    case WhoToGreet(who) => greeting = s"hello, $who"
      //imperatif
    case Greet => sender ! Greeting(greeting) // Send the current greeting back to the sender
      // Envoyer un message de salutation a l'envoyeur
    case Banner(s)  =>   sender ! Greeting(s"*** welcome to america ***")
  }
}

class Anagram extends Actor{
    val lexique = scala.io.Source.fromFile("/Users/gisiotabera/Downloads/anagramme.txt","utf-8").getLines.toList

    def lettres(mot: String): Map[Char, Int] = {
      mot.filter(_.isLetter).groupBy((a:Char) => a).mapValues(_.length)
    }

    def inf(x : Map[Char, Int], y:Map[Char, Int]) : Boolean = {
      x.keys.forall((a : Char) => x(a) <= y.getOrElse(a,0) )
    }

    val index = lexique.groupBy(lettres)

    def receive = {
      case Chercher(mot) => {
        sender ! ListeAnagrammes(index.getOrElse(lettres(mot), List()))
      }
      case Clavier(touches:String) => {
        val superclavier = lettres(touches).mapValues(_ * 100)
        val idx = index.keys.filter((bag : Map[Char, Int]) => inf(bag, superclavier))

        val n = (for {
          k <- idx
          l <- index(k)
        } yield l.length).sum
        //index.filter((bag:Map[Char, Int]) => Set)
        sender! Nombre(45)
      }
    }
}



object HelloAkkaScala extends App {

  // Create the 'helloakka' actor system (système d'acteur)
  val system = ActorSystem("helloakka")
  // tous les restes soient

  // Create the 'greeter' actor
  val greeter = system.actorOf(Props[Greeter], "greeter")

  val anagram = system.actorOf(Props[Anagram], "anagram")

  // Create an "actor-in-a-box"
  val inbox = Inbox.create(system)

  // Tell the 'greeter' to change its 'greeting' message
  greeter.tell(WhoToGreet("akka"), ActorRef.noSender)

  // Ask the 'greeter for the latest 'greeting'
  // Reply should go to the "actor-in-a-box"
  inbox.send(greeter, Greet)

  // Wait 5 seconds for the reply with the 'greeting' message
  val Greeting(message1) = inbox.receive(5.seconds)
  println(s"Greeting: $message1")

  inbox.send(anagram, Chercher("mange"))
  val ListeAnagrammes(liste) = inbox.receive(60.seconds)
  println(liste)

  inbox.send(anagram, Clavier("auie"))
  val Nombre(n1) = inbox.receive(60.seconds)
  println(s"Resultat 1 $n1")

  inbox.send(anagram, Clavier("q"))
  val Nombre(n2) =inbox.receive(60.seconds)
  println(s"Resultat 2 $n2")

  // Change the greeting and ask for it again
  greeter.tell(WhoToGreet("typesafe"), ActorRef.noSender)

  inbox.send(greeter, Greet)
  val Greeting(message2) = inbox.receive(5.seconds)
  println(s"Greeting: $message2")

  val greetPrinter = system.actorOf(Props[GreetPrinter])
  // after zero seconds, send a Greet message every second to the greeter with a sender of the greetPrinter
  system.scheduler.schedule(0.seconds, 1.second, greeter, Banner)(system.dispatcher, greetPrinter)



  
}

// prints a greeting
class GreetPrinter extends Actor {
  def receive = {
    case Greeting(message) => println(message)
  }
}