import java.security.MessageDigest;
import scala.math
import java.util.Arrays;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import java.util.Random
import java.math
import javax.crypto.spec.SecretKeySpec;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
object AES {
		var IV  = "AAAAAAAAAAAAAAAA";

	def encrypt(plainText:String,encryptionKey:String):Array[Byte]={
			var cipher:Cipher= Cipher.getInstance("AES/CBC/PKCS5Padding", "SunJCE");
			var key: SecretKeySpec = new SecretKeySpec(encryptionKey.getBytes("UTF-8"), "AES");
			cipher.init(Cipher.ENCRYPT_MODE, key,new IvParameterSpec(IV.getBytes("UTF-8")));
			return cipher.doFinal(plainText.getBytes("UTF-8"));
  }
		
  
   def decrypt(cipherText:Array[Byte], encryptionKey:String):String={
		var cipher: Cipher= Cipher.getInstance("AES/CBC/PKCS5Padding", "SunJCE");
		var key:SecretKeySpec= new SecretKeySpec(encryptionKey.getBytes("UTF-8"), "AES");
		cipher.init(Cipher.DECRYPT_MODE, key,new IvParameterSpec(IV.getBytes("UTF-8")));
		return new String(cipher.doFinal(cipherText),"UTF-8");
   }
	def generatekey():String={
			var i:BigInt= BigInt("1000000000000000")
			var k:BigInt= 0
			var ran = new Random()
			while(i>0)
			{
			  k += i*(ran.nextInt(8)+1)
			  i = i / 10
			}
			var s = k.toString
			return s

	 	}
	def genIntKey():String={
			var i:BigInt= BigInt("1000000000000000")
			var k:BigInt= 0
			var ran = new Random()
			while(i>0)
			{
			  k += i*(ran.nextInt(8)+1)
			  i = i / 10
			}
			var s = k.toString
			return s
			
	}
}

