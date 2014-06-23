import akka.actor._
import java.util.Random
import scala.collection.mutable.HashMap

case class StartOta
case class Start_Actors(a:ActorRef)
case class EstCon(m:Int, a:ActorRef, b:ActorRef, Nonce:Array[Byte], enM: Array[Byte], enA:Array[Byte], enB:Array[Byte])
case class Start_SimulationOta(a:ActorRef)
case class CheckOta(M:Int, a:ActorRef , b:ActorRef , enNa:Array[Byte], enM:Array[Byte], enA:Array[Byte], enB:Array[Byte], enNb:Array[Byte],enMb:Array[Byte],enAb:Array[Byte],enBb:Array[Byte])
case class CheckedOta(M:Int, Naka:Array[Byte], ses_keyka:Array[Byte], Nbkb:Array[Byte], ses_keykb:Array[Byte])
case class Connect(m:Int, na:Array[Byte] , seskeyka: Array[Byte])
case class mess(a:Array[Byte])
object otwayrees {
	def main(args : Array[String]):Unit={
	  val system = ActorSystem("SecuritySystem")
	  val kdc = system.actorOf(Props[KDC])
	  kdc ! StartOta
	}
}
class KDC extends Actor{
	val KAS = "KeyDistribution1"
	val KBS = "KeyDistribution2"  
	val A = context.actorOf(Props(new A(self:ActorRef)))
	val B = context.actorOf(Props(new B(self:ActorRef)))
	var map = new HashMap[ActorRef,String]
	var ses_key = ""
	var rand = new Random()
	def receive ={
	  case StartOta=>
	    map += (A -> KAS)
	    map += (B -> KBS)
	    A ! Start_SimulationOta(B)
	  case CheckOta(m, a , b , enNa, enM, enA, enB, enNb,enMb,enAb,enBb)=>
	    var Na = AES.decrypt(enNa, KAS)
	    var M = AES.decrypt(enM, KAS)
	    var Aa = AES.decrypt(enA, KAS)
	    var Ba = AES.decrypt(enB, KAS)
	    var Nb = AES.decrypt(enNb, KBS)
	    var Mb = AES.decrypt(enMb, KBS)
	    var Bb = AES.decrypt(enBb, KBS)
	    var Ab = AES.decrypt(enAb, KBS)
	    
	    if((Ba == B.toString) && (Ab== A.toString))
	    {
	    	ses_key = AES.generatekey
	    	//println(ses_key)
	    	var Naka = AES.encrypt(Na, KAS)
	    	var ses_keyka = AES.encrypt(ses_key, KAS)
	    	var Nbkb = AES.encrypt(Nb, KBS)
	    	var ses_keykb = AES.encrypt(ses_key, KBS)
	    	println("Server Sending Encrpted Session key AB to B: "+ses_keyka)
	    	B!CheckedOta(M.toInt, Naka, ses_keyka, Nbkb, ses_keykb)
	    	
	    }
	 case "Complete"=> 
	 		context.system.shutdown
	}
}
class A(boss:ActorRef) extends Actor{
	val KAS = "KeyDistribution1"
	var b:ActorRef = self
	var SK = 0
	var rand = new Random()
	var Na = rand.nextInt()
	var M = 0
	var ses_key= ""
	def receive={
	  case Start_SimulationOta(han)=>
	  {
		  b = han
		  M = rand.nextInt()
		  var enNa = AES.encrypt(Na.toString, KAS)
		  var enM = AES.encrypt(M.toString, KAS)
		  var enA = AES.encrypt(self.toString,KAS)
		  var enB = AES.encrypt(b.toString,KAS)
		  println("Simulation Started")
		  b ! EstCon(M,self,b,enNa,enM,enA,enB)
	  }
	  case Connect(m, na, seskeyka)=>
	    var n = AES.decrypt(na, KAS)
	    if(n.toInt == Na)
	    {
	      ses_key = AES.decrypt(seskeyka,KAS)
	      println("Session key at A "+ ses_key)
	      println("Connection succesfully established")
	      var msg = AES.encrypt("Hihowryou",ses_key)
	      b!mess(msg)
	    }
	    else
	      println("Replay Attack")
	      boss!"Complete"
	 case EstCon(m, senta, a, enNa, enM, enA, enB)=>
    	M = m
    	b = senta
    	Na = rand.nextInt()
    	var enNb = AES.encrypt(Na.toString, KAS)
    	var enMb = AES.encrypt(M.toString, KAS)
    	var enAb = AES.encrypt(a.toString(),KAS)
    	var enBb = AES.encrypt(self.toString(),KAS)
    	boss ! CheckOta(M, a , self , enNa, enM, enA, enB, enNb,enMb,enAb,enBb)
    	
	case CheckedOta(m, na, ses_keyka, nb, ses_keykb)=>
    	var n = AES.decrypt(nb, KAS)
    	if(n.toInt == Na)
    	 {
    		ses_key = AES.decrypt(ses_keykb,KAS)
    		println("Session key at B  decrypted with KBC  "+ses_key)
    		b!Connect(m, na, ses_keyka)
    	 }
	    else 
	      println("Replay Attack")
	      sender!"Complete"
    case mess(a)=>
    	println("In A"+AES.decrypt(a,ses_key))
    
	}
  
}
class B(boss:ActorRef) extends Actor{
	val KBS = "KeyDistribution2"
	var a:ActorRef = null
	var SK = 0
	var rand = new Random()
	var Nb = rand.nextInt()
	var M = 0
	var ses_key = ""
	
  def receive={
	  case Start_SimulationOta(han)=>
	  {
		  a = han
		  M = rand.nextInt()
		  var enNb = AES.encrypt(Nb.toString, KBS)
		  var enM = AES.encrypt(M.toString, KBS)
		  var enA = AES.encrypt(self.toString,KBS)
		  var enB = AES.encrypt(a.toString,KBS)
		  a ! EstCon(M,self,a,enNb,enM,enB,enA)
	  }
	  case Connect(m, na, seskeyka)=>
	    var n = AES.decrypt(na, KBS)
	    if(n.toInt == Nb)
	    {
	      ses_key = AES.decrypt(seskeyka,KBS)
	      println("Connection succesfully established")
	      println(ses_key)
	      var msg = AES.encrypt("Hihowryou",ses_key)
	      a!mess(msg)
	      boss!"Complete"
	    }
    case EstCon(m, senta, b, enNa, enM, enA, enB)=>
    	M = m
    	a = senta
    	Nb = rand.nextInt()
    	var enNb = AES.encrypt(Nb.toString, KBS)
    	var enMb = AES.encrypt(M.toString, KBS)
    	var enAb = AES.encrypt(a.toString(),KBS)
    	var enBb = AES.encrypt(self.toString(),KBS)
    	println("Message received B. Processing...")
    	boss ! CheckOta(M, a , self , enNa, enM, enA, enB, enNb,enMb,enAb,enBb)
    case CheckedOta(m, na, ses_keyka, nb, ses_keykb)=>
    	var n = AES.decrypt(nb, KBS)
    	if(n.toInt == Nb)
    	 {
    		ses_key = AES.decrypt(ses_keykb,KBS)
    		println("Session key at B "+ses_key)
    		a!Connect(m, na, ses_keyka)
    	 }
	    else 
	      println("Replay Attack")
	      sender!"Complete"
    case mess(a)=>
    	//println("In B"+AES.decrypt(a,ses_key))
    	sender!"Complete"
  }
}