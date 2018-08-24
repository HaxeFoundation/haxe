package haxe.crypto.padding;

import haxe.io.BytesBuffer;
import haxe.io.Bytes;

class TBC 
{
    public static function pad(ciphertext:Bytes, blockSize:Int):Bytes
    {
        var tb = ciphertext.get(ciphertext.length-1) & 1;
        var paddingByte = (tb == 1)?0x00:0xFF;
        var buffer: BytesBuffer = new BytesBuffer();
        buffer.addBytes(ciphertext,0,ciphertext.length);
        var padding:Int = blockSize - ciphertext.length % blockSize;
        for(i in 0...padding) {
          buffer.addByte(paddingByte); 
        }
        return buffer.getBytes();
    }

    public static function unpad(encrypt:Bytes):Bytes
    {
        var pos = encrypt.length-1;
        var paddingByte = encrypt.get(pos);
        var padding : Int = paddingByte;
        while ( padding == paddingByte && pos > -1) {
            pos--;
            padding = encrypt.get(pos);
        }
        return encrypt.sub(0,pos+1);
    }
}
