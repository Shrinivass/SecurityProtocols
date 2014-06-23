import java.math.BigInteger;
import java.security.SecureRandom;

object RSA {
   val one:BigInteger       = new BigInteger("1");
   val random:SecureRandom 	= new SecureRandom();
   var privateKey: BigInteger = new BigInteger("0")
   var publicKey: BigInteger = new BigInteger("0")
   var modulus: BigInteger = new BigInteger("0")

   // generate an N-bit (roughly) public and private key
   def RSA() {
      var p:BigInteger = BigInteger.probablePrime(256, random);
      var q:BigInteger  = BigInteger.probablePrime(256, random);
      var phi:BigInteger = (p.subtract(one)).multiply(q.subtract(one));

      modulus    = p.multiply(q);                                  
      //publicKey  = new BigInteger("65537");     // common value in practice = 2^16 + 1
      publicKey  = BigInteger.probablePrime(16, random);     // common value in practice = 2^16 + 1
      privateKey = publicKey.modInverse(phi);
   }


   def encryptRSA(message:BigInteger,puKey:BigInteger,n:BigInteger):BigInteger= {
	  
      return message.modPow(puKey, n);
   }

   def decryptRSA(encrypted:BigInteger,prKey:BigInteger,n:BigInteger):BigInteger={
      return encrypted.modPow(prKey, n);
   }

   def convertstring:String ={
      var s:String  = "";
      s += "public  = " + publicKey  + "\n";
      s += "private = " + privateKey + "\n";
      s += "modulus = " + modulus;
      return s;
   }
 
   def main(args:Array[String]):Unit= {

   }
}
