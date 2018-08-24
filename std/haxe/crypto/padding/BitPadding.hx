package haxe.crypto.padding;

import haxe.io.BytesBuffer;
import haxe.io.Bytes;

class BitPadding 
{
    public static  function pad(ciphertext:Bytes, blockSize:Int):Bytes
    {
        var buffer: BytesBuffer = new BytesBuffer();
        buffer.addBytes(ciphertext,0,ciphertext.length);
        buffer.addByte(0x80);
        var padding:Int = blockSize - ciphertext.length % blockSize -1;
        for(i in 0...padding) {
          buffer.addByte(0x00); 
        }
        return buffer.getBytes();
    }

    public static  function unpad(encrypt:Bytes):Bytes
    {
        var padding : Int = 0;
        var pos = encrypt.length-1;
        while ( padding != 0x80 && pos > -1) {
          padding = encrypt.get(pos);
          pos--;
        }
        return encrypt.sub(0,pos+1);
    }
}
