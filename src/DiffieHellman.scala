import akka.actor._
import scala.math._ //random / pow / BigInt

case object StartDif
case class Init(g:Int, p: Int, PUkey:Int)
case class PublicKey(key:Int)

object Main {
	def main(args: Array[String]): Unit = {
	  val system = ActorSystem("DiffeHellman")
	  val alice = system.actorOf(Props[Alice])

	  alice ! StartDif
	}
}

class Alice extends Actor{
	val generator:Int = 2
	val primeModular:Int = 17
	val PRalice= (random*100).toInt
	val PUalice = ((BigInt(generator) pow PRalice) % primeModular).toInt
	var PUbob = 0

	var secretkey=0

	val bob = context.actorOf(Props[Bob])

	def receive ={
		case StartDif=>{
			println("Init message to Bob, generator = "+ generator+
				" , primeModular = "+ primeModular +
				" PUalice = "+ PUalice)
			bob ! Init(generator,primeModular,PUalice)
		}
		case in:PublicKey=>{
			PUbob = in.key
			secretkey = ((BigInt(PUbob) pow PRalice) % primeModular).toInt
			println("Alice received PUbob = "+PUbob+", session key = "+secretkey)
			context.system.shutdown

		}
	}
}

class Bob extends Actor{
	var generator = 0
	var primeModular = 0
	val PRbob= (random*100).toInt
	var PUbob = 0
	var PUalice = 0
	var secretkey = 0

	def receive ={
		case in:Init=>{
			generator = in.g
			primeModular = in.p
			PUalice = in.PUkey

			PUbob = ((BigInt(generator) pow PRbob) % primeModular).toInt
			secretkey = ((BigInt(PUalice) pow PRbob) % primeModular).toInt

			println("Bob received PUalice, session key= "+secretkey)
			sender ! PublicKey(PUbob)
		}
	}
}