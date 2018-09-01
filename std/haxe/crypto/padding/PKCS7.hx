package haxe.crypto.padding;

import haxe.io.BytesBuffer;
import haxe.io.Bytes;

class PKCS7
{
    public static function pad(ciphertext:Bytes, blockSize:Int):Bytes
    {
        if ( blockSize > 255 ) throw "PKCS#7 padding cannot be longer than 255 bytes";
        if ( blockSize < 0 ) throw "PKCS#7 padding size must be positive";
        var buffer: BytesBuffer = new BytesBuffer();
        buffer.addBytes(ciphertext,0,ciphertext.length);
        var padding:Int = blockSize - ciphertext.length % blockSize;
        for(i in 0...padding) {
          buffer.addByte(padding & 0xFF); 
        }
        return buffer.getBytes();
    }

    public static  function unpad(encrypt:Bytes):Bytes
    {
      var padding : Int = encrypt.get(encrypt.length-1);
      if ( padding > encrypt.length ) throw "Cannot remove "+padding+" bytes, because message is "+encrypt.length+" bytes";
      var block = encrypt.length - padding;
      for(i in block...encrypt.length) {
          if (encrypt.get(i) != padding ) throw "Invalid padding value. Got "+encrypt.get(i)+", expected "+padding+" at position "+ i;
      }
      return encrypt.sub(0,block);
    }
}