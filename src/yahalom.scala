import akka.actor._
import java.util.Random
import scala.collection.mutable.HashMap

case class StartYaha
case class Conyaha(a:ActorRef, Nonce:Int)
case class Start_SimulationYaha(a:ActorRef)
case class CheckYa(self:ActorRef, enA:Array[Byte], enNa:Array[Byte], enNb:Array[Byte])
case class verifyYa(BKa:Array[Byte], ses_keyKa:Array[Byte], NaKa:Array[Byte],Nbse:Array[Byte], Akb:Array[Byte], Kabkb:Array[Byte])	      
case class estYa(nbse:Array[Byte], akb:Array[Byte],kabkb:Array[Byte])
object Yahalom {
	def main(args : Array[String]):Unit={
	  val system = ActorSystem("SecuritySystem")
	  val kdc = system.actorOf(Props[KDCYaha])
	  kdc ! StartYaha
	}
}
class KDCYaha extends Actor{
	val KAS = "KeyDistribution1"
	val KBS = "KeyDistribution2"  
	val A = context.actorOf(Props(new AYaha(self:ActorRef)))
	val B = context.actorOf(Props(new BYaha(self:ActorRef)))
	var map = new HashMap[ActorRef,String]
	var ses_key = ""
	var rand = new Random()
	def receive ={
	  case StartYaha=>
	    map += (A -> KAS)
	    map += (B -> KBS)
	    println("Started")
	    A ! Start_SimulationYaha(B)
	  case CheckYa(sentb, enA, enNa, enNb)=>
	    if(map.contains(sentb))
	    {
	      println("Sending message from erver to A")
	      var senta= AES.decrypt(enA, KBS)
	      var deNa = AES.decrypt(enNa, KBS)
	      var deNb = AES.decrypt(enNb, KBS)
	      ses_key = AES.generatekey
	      var BKa = AES.encrypt(sentb.toString, KAS)
	      var ses_keyKa = AES.encrypt(ses_key, KAS)
	      var Naka = AES.encrypt(deNa, KAS)
	      var Nbse = AES.encrypt(deNb, ses_key)
	      var Akb = AES.encrypt(senta.toString(), KBS)
	      var Kabkb = AES.encrypt(ses_key, KBS)
	      A!verifyYa(BKa, ses_keyKa, Naka,Nbse, Akb, Kabkb)
	    }
	    else
	      println("mismatch")
	    
	 case "completeYa"=> 
	 		context.system.shutdown
	}
}
class AYaha(boss:ActorRef) extends Actor{
	val KAS = "KeyDistribution1"
	var b:ActorRef = self
	var SK = 0
	var rand = new Random()
	var Na = rand.nextInt()
	var ses_key= ""
	def receive={
	  case Start_SimulationYaha(han)=>
	  {
		  b = han
		  println("Yahalom")
		  println("Nonce of A "+Na)
		  b ! Conyaha(self,Na)
	  }
	  case verifyYa(bKa, ses_keyKa, naKa,nbse, akb, kabkb)=>
	    	var deb = AES.decrypt(bKa, KAS)
	    	var ses_key = AES.decrypt(ses_keyKa, KAS)
	    	
	    	var deNa = AES.decrypt(naKa, KAS)
	    	println("Nonce of A in msg "+deNa)
	    	println("Session Key at A "+ses_key)
	    	if(b.toString==deb)
	    	{
	    	  if(deNa == Na.toString)
	    	  {
	    	    b!estYa(nbse, akb,kabkb)
	    	  }
	    	  else
	    	    println("Replay Attack")
	    	}
	    	else
	    	  println("Mismatch")
	    	  sender!"completeYa"
	      
	 	}
  
}
class BYaha(boss:ActorRef) extends Actor{
	val KBS = "KeyDistribution2"
	var a:ActorRef = null
	var SK = 0
	var rand = new Random()
	var Nb = rand.nextInt()
	var M = 0
	var ses_key = ""
	
  def receive={
	  case Conyaha(senta, na)=>
	    a = senta
	    var enA = AES.encrypt(a.toString, KBS)
	    var enNb = AES.encrypt(Nb.toString, KBS)
	    var enNa = AES.encrypt(na.toString, KBS)
	    println("Nonce of B "+Nb)
	    boss ! CheckYa(self, enA, enNa, enNb)
	  case estYa(nbse, akb,kabkb)=>
	    var ses_key = AES.decrypt(kabkb, KBS)
	    var denb = AES.decrypt(nbse, ses_key)
	    if(denb == Nb.toString)
	    {
	      println("Nonce of B after receiving message "+denb)
	      println("Session key at B  "+ses_key)
	      println("Connection Established")
	      boss!"completeYa"
	    }
	    else
	      println("Replay Attack in A")
	      boss!"completeYa"
	    }
}