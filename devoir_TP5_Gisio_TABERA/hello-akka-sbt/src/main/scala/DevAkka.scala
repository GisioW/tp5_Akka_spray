import spray.routing.HttpService
import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

import akka.actor.Actor
class PresidentActor extends Actor with PresideService {
  def actorRefFactory = context
  def receive = runRoute(myRoute)
}

// A mettre dans boot
object Boot extends App {
  implicit val system = ActorSystem("on-spray-can")
  val service = system.actorOf(Props[PresidentActor], "demo-service")
  implicit val timeout = Timeout(5.seconds)
  IO(Http) ? Http.Bind(service, interface = "localhost", port = 8080)
}


case class President(nom : String, pays: String, message : String)
object PresidentProtocol extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val userFormat = jsonFormat3(President.apply)

}



trait PresidentDao {

  def creerPresident(president: President) : String = {
    "Nouveau president Elu. " + "\n Nom : " + president.nom + "\n Pays: " + president.pays + "\n Quote : "+ president.message+ "\n"
  }

  def supprimerPresident(nom: String) : String = {
    "Supprimer un president "+nom +"\n"
  }

  def getPresidentByName(nom : String): President = nom match {
    case "Holland" => President("Hollande","France", "Pourquoi Grownland me critque souvent?")
    case "Obama" => President("Obama", "USA","I must go man so I must take pleasure of my days president")
    case "Vladimir" => President("Vladimir","Russie","Moi Putine pas peur de personne, Moi elimine tout ce qui me gene")
    case _ => President("None","None","None")
  }

}


// A mettre dans service
trait PresidentService extends HttpService with PresidentDao {

  val myRoute =
    pathPrefix("president") {
      path("get"/Segment){ nom =>
        get{
          complete{
            getPresidentByName(nom)
          }
        }
      }~
        path("add"){
          post{
            entity(as[President]){ president: President  =>
              complete{
                creerPresident(president)
              }
            }
          }
        }~
        path("delete"/Segment){ nom =>
          delete{
            complete{
              supprimerPresident(nom)
            }
          }
        }
    }


}

