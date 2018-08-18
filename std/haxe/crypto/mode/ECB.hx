package haxe.crypto.mode;

import haxe.io.Bytes;

class ECB
{
    public static function encrypt( src : Bytes, blockSize : Int, encryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
        var i : Int = 0;
        var len : Int = src.length;
	while (i < len)
	{
	  encryptBlock(src, i, src , i);
	  i += blockSize;
	}
    }

    public static function decrypt( src : Bytes, blockSize : Int, decryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
        var i : Int = 0;
        var len : Int = src.length;
	while (i < len)
	{
	   decryptBlock(src, i, src , i);
	   i += blockSize;
	}
    }
}
