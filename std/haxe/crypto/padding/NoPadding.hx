package haxe.crypto.padding;

import haxe.io.Bytes;

class NoPadding 
{
    public static  function pad(ciphertext:Bytes, blockSize:Int):Bytes
    {
        return ciphertext.sub(0,ciphertext.length);
    }

    public static  function unpad(encrypt:Bytes):Bytes
    {
      return encrypt;
    }
}
