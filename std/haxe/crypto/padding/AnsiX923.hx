package haxe.crypto.padding;

import haxe.io.BytesBuffer;
import haxe.io.Bytes;

class AnsiX923
{
    public static  function pad(ciphertext:Bytes, blockSize:Int):Bytes
    {
        var buffer: BytesBuffer = new BytesBuffer();
        buffer.addBytes(ciphertext,0,ciphertext.length);
        var padding:Int = blockSize - ciphertext.length % blockSize -1;
        for(i in 0...padding) {
          buffer.addByte(0x00); 
        }
        buffer.addByte(padding+1);
        return buffer.getBytes();
    }

    public static function unpad(encrypt:Bytes):Bytes
    {
      var padding : Int = encrypt.get(encrypt.length-1);
      return encrypt.sub(0,encrypt.length - padding);
    }
}
