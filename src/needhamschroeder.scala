import akka.actor._
import java.util.Random
import scala.collection.mutable.HashMap

case class StartNeed
case class EstConneed(a:ActorRef, b:ActorRef, Nonce:Array[Byte])
case class Start_Simulationneed(a:ActorRef)
case class ConSend(Naka:Array[Byte], ses_keyka:Array[Byte], enBb:Array[Byte], ses_keykb:Array[Byte], enAb:Array[Byte])
case class connectneed(Na:Array[Byte], ses_keykb:Array[Byte], enAb:ActorRef)
case class Checkneed(ennew:Array[Byte],enNb:Array[Byte])
case class verify(ennew:Array[Byte])
object needhamschroder {
	def main(args : Array[String]):Unit={
	  val system = ActorSystem("SecuritySystem")
	  val kdc = system.actorOf(Props[KDCNeed])
	  kdc ! StartNeed
	}
}
class KDCNeed extends Actor{
	val KAS = "KeyDistribution1"
	val KBS = "KeyDistribution2"  
	val A = context.actorOf(Props(new Aneed(self:ActorRef)))
	val B = context.actorOf(Props(new Bneed(self:ActorRef)))
	var map = new HashMap[ActorRef,String]
	var ses_key = ""
	var rand = new Random()
	def receive ={
	  case StartNeed=>
	    map += (A -> KAS)
	    map += (B -> KBS)
	    println("Started")
	    A ! Start_Simulationneed(B)
	  case EstConneed(senta,sentb,enNa)=>
	    if(map.contains(senta))
	    {
	      if(map.contains(sentb))
	      {
	    	var Na = AES.decrypt(enNa, KAS)
	        ses_key = AES.generatekey
	    	println(ses_key)
	    	var Naka = AES.encrypt(Na, KAS)
	    	var ses_keyka = AES.encrypt(ses_key, KAS)
	    	var ses_keykb = AES.encrypt(ses_key, KBS)
	    	var enAb = AES.encrypt(A.toString(),KBS)
	    	var enBb = AES.encrypt(B.toString(),KAS)
	    	sender ! ConSend(Naka, ses_keyka, enBb, ses_keykb, enAb)
    	  }
	    }
	    else
	      println("Mismatch")
	      context.system.shutdown
	  
	 case "completeneed"=> 
	 		context.system.shutdown
	}
}
class Aneed(boss:ActorRef) extends Actor{
	val KAS = "KeyDistribution1"
	var b:ActorRef = self
	var SK = 0
	var rand = new Random()
	var Na = rand.nextInt()
	var ses_key= ""
	def receive={
	  case Start_Simulationneed(han)=>
	  {
		  b = han
		  var enNa = AES.encrypt(Na.toString, KAS)
		  var enA = AES.encrypt(self.toString,KAS)
		  var enB = AES.encrypt(b.toString,KAS)
		  println("Starting Simulaiton for Needham")
		  boss ! EstConneed(self,b,enNa)
	  }
	  case ConSend(naka, ses_keyka, enBb, ses_keykb, enAb)=>
	    	
	    	var denaka = AES.decrypt(naka,KAS)
	    	if(Na.toString==denaka)
	    	{
	    		ses_key = AES.decrypt(ses_keyka,KAS)
	    		Na = rand.nextInt
	    		var enNa = AES.encrypt(Na.toString, ses_key)
	    		println("A sending message to B")
	    		println("Nonce Value at A  "+Na)
	    		b ! connectneed(enNa,ses_keykb, self)
	    	}
	    	else
	    	  println("Replay Attack")
	  case Checkneed(en, enNb)=>
	    	var deen = AES.decrypt(en, ses_key).toInt
	    	if(deen == Na-1)
	    	{
	    	
	    	var deNb = AES.decrypt(enNb, ses_key).toInt-1
	    	 println("Nonce Value at A after subtraction  "+deNb)
	    	var ennew = AES.encrypt(deNb.toString,ses_key)
	    	sender ! verify(ennew)
	    	}
	    	else
	    	  boss! "completeneed"
	  case Connect(m, na, seskeyka)=>
	    var n = AES.decrypt(na, KAS)
	    if(n.toInt == Na)
	    {
	      ses_key = AES.decrypt(seskeyka,KAS)
	      println("Connection succesfully established")
	      println(ses_key)
	      var msg = AES.encrypt("Hihowryou",ses_key)
	      b!mess(msg)
	      boss!"completeneed"
	    }
	    else
	      println("Replay Attack")
	      boss!"completeneed"
	}
  
}
class Bneed(boss:ActorRef) extends Actor{
	val KBS = "KeyDistribution2"
	var a:ActorRef = null
	var SK = 0
	var rand = new Random()
	var Nb = rand.nextInt()
	var M = 0
	var ses_key = ""
	
  def receive={
	  case connectneed(na,ses_keykb, enAb)=>
	    ses_key = AES.decrypt(ses_keykb, KBS)
	    var dena= AES.decrypt(na,ses_key).toInt-1
	    var ennew = AES.encrypt(dena.toString,ses_key)
	    a = enAb
	    println("Nonce Value at B after subtracting 1  "+dena)
	    println("Nonce value of B  "+Nb)
	    var enNb= AES.encrypt(Nb.toString, ses_key)
	    sender ! Checkneed(ennew, enNb)
	  case verify(ennew)=>
	    var denew = AES.decrypt(ennew,ses_key).toInt
	   	if(Nb-1 == denew)
	   	 {
	      println("Log In successful")
	      boss!"completeneed"
	   	 }
	   	else
	   	{
	      println("Denied")
	      boss!"completeneed"
	   	}
  }
}