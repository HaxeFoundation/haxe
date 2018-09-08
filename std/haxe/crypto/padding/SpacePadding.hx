package haxe.crypto.padding;

import haxe.io.BytesBuffer;
import haxe.io.Bytes;

class SpacePadding
{
    public static  function pad(ciphertext:Bytes, blockSize:Int):Bytes
    {
        var buffer: BytesBuffer = new BytesBuffer();
        buffer.addBytes(ciphertext,0,ciphertext.length);
        var padding:Int = blockSize - ciphertext.length % blockSize;
        for(i in 0...padding) {
          buffer.addByte(0x20); 
        }
        return buffer.getBytes();
    }

    public static function unpad(encrypt:Bytes):Bytes
    {
        var padding : Int = 0x20;
        var pos = encrypt.length;
        while ( padding == 0x20 && pos > 0) {
            pos--;
            padding = encrypt.get(pos);
        }
        return encrypt.sub(0,pos+1);
    }
}
