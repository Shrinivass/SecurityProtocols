import akka.actor._
import scala.collection.mutable.HashMap
import java.math.BigInteger
import java.security.SecureRandom;
import java.util.Random
//import java.
//contorl msg
case class Start(nodeNumber:Int)
case object Create
case object Initiate
case object LoginSuccess
case class Wake(userID:Int)

//actor msg
case object reqPUserver
case class ackPUserver(PUserver:BigInteger, ServerN:BigInteger)


case class reqRegistration(encrypted_userID:BigInteger,PUuser:BigInteger, ClientN: BigInteger)
case class ackRegistration

case class askLogin(ID:BigInteger, PUuser:BigInteger, ClientN: BigInteger)
case class ackLogin
case class challenge(chall: BigInteger, PUserver:BigInteger, ServerN:BigInteger, str:Array[Byte])
case class check(enchal : Array[Byte], ID : BigInteger)

object modifiedchallengeresponse {

	def main(args : Array[String]):Unit={
	  val nodeNumber = 1
	  val system = ActorSystem("SecuritySystem")
	  val server = system.actorOf(Props[Server])
	  server ! Start(nodeNumber)
	}
}

class Server extends Actor{
	var map = new HashMap[BigInteger,BigInteger]
	var challmap = new HashMap[BigInteger, String]
	var lock = 0
	var ses_key = ""
	RSA.RSA()
	val PRserver = RSA.privateKey
	val PUserver = RSA.publicKey
	val ServerN = RSA.modulus

	var nodeNumber = 0
	var createCnt = 0
	var loginCnt = 0
	def receive ={
		case in:Start =>{
			nodeNumber = in.nodeNumber
			createCnt = in.nodeNumber
			//println("Public and private keys"+PRserver)
			//println(PUserver)
			//println(ServerN)
			context.self ! Create
		}
		case Create =>{
			val client = context.actorOf(Props[Client])
			client ! Wake(createCnt)
			//println("Create"+createCnt)
			createCnt -= 1
			if(createCnt >0) context.self ! Create
			//println("casdfa"+createCnt)
		}
		case `reqPUserver` =>{
			//println("Req Persew")
			sender ! ackPUserver(PUserver,ServerN)
		}
		case in:reqRegistration=>{
			val msg = RSA.decryptRSA(in.encrypted_userID,PUserver, ServerN)
			val userID = RSA.decryptRSA(msg,in.PUuser,in.ClientN)
			//println("User IDDSAD"+userID)
			val PUuser = in.PUuser
			map += (userID -> PUuser)

			sender ! ackRegistration
		}
		case in:askLogin=>{
			 //<-----------decrytion of userID
		  if(lock == 0)
		  {
			val msg = RSA.decryptRSA(in.ID,PUserver, ServerN)
			val userID = RSA.decryptRSA(msg,in.PUuser,in.ClientN)
			//println("Inasklogin"+userID)
			//println(map)
			
			if(map.contains(userID)) 
			  {
				val rand = new Random
				//var chal = rand.nextInt(9)*rand.nextInt(9)
				//var chalBig = new BigInteger(chal.toString)
				ses_key = AES.genIntKey.toString()
				println("Challenge produced "+ses_key)
				var chalBig = new BigInteger(ses_key)
				//println("Chal"+chalBig)
				var chall = RSA.encryptRSA(chalBig, in.PUuser, in.ClientN)
				challmap += (userID-> ses_key)
				var str = AES.encrypt(self.toString,ses_key)
				//println("Challenge there"+chall+"   0"+userID)
				sender ! challenge(chall,PUserver,ServerN,str)
				//sender ! ackLogin
				//println("asdf"+challmap) 
			  }
		  }
		}
		case in:check=>{
			loginCnt += 1
			val dechal = AES.decrypt(in.enchal, ses_key)
			var bechal:BigInt = BigInt(dechal)+1
			println("CHallenge resolution "+bechal)
			
			if(challmap(in.ID) == bechal.toString)
			  println("LogIn Sucessful")
			else 
			  println("Failed")
			if(loginCnt == nodeNumber) context.system.shutdown
		}
		case LoginSuccess =>{
			loginCnt += 1 
			if(loginCnt == nodeNumber) context.system.shutdown
		}
	}

}

class Client extends Actor{
	RSA.RSA()
	val PRuser = RSA.privateKey 
	val PUuser = RSA.publicKey
	val ClientN = RSA.modulus
	var enID = new BigInteger("0") 
	var PUserver = new BigInteger("0")
	var ServerN = new BigInteger("0")
	var ID = new BigInteger("0")
	var ses_key = ""

	def receive ={
		case in:Wake=>{
			var str = in.userID.toString
			ID = new BigInteger(str)
			//println("ID  "+ID)
			sender ! reqPUserver
		}
		case in:ackPUserver=>{
			PUserver= in.PUserver
			ServerN= in. ServerN
			val msg = RSA.encryptRSA(ID, PRuser, ClientN)
			enID = RSA.encryptRSA(msg, PUserver, ServerN)
			////println("Sent user id"+enID)
			sender ! reqRegistration(enID, PUuser, ClientN) //<-----------(encryption of ID, PUuser)
		}
		case `ackRegistration`=>{
			//<-----------encrypt userID
			println("A initiating login")
		  	sender ! askLogin(enID, PUuser, ClientN)
		}
		case in:challenge=>{
			
			val chal = RSA.decryptRSA(in.chall, PRuser, ClientN)
			//println("Challenge here"+in.chall+"  "+ID)
			ses_key = chal.toString
			println("Challenge acceptance  "+ses_key)
			var b = AES.decrypt(in.str,ses_key)
			if(b==sender.toString)
			{
			 val enchal = RSA.encryptRSA(chal,PUserver,in.ServerN)
			//println("Decrypted"+chal)
			var b:BigInt = BigInt(ses_key)
			b = b-1
			var asd = AES.encrypt(b.toString,ses_key)
			//println(AES.decrypt(asd, ses_key))
			sender ! check(asd, ID)
			}
		}
		case ackLogin=>{
			//println("Am in login")
			sender ! LoginSuccess
		}
	}

}