package haxe.crypto.mode;

import haxe.io.Bytes;

class OFB
{
    public static function encrypt( src : Bytes, iv : Bytes, blockSize : Int, encryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
	var vector = iv.sub(0,iv.length);
	var i : Int = 0;
	var len : Int = src.length;
	var chiperText : Bytes;
	while (i < len)
	{
            encryptBlock(vector, 0, vector , 0);
            chiperText = vector.sub(0,vector.length);
            var block:Int = (i+blockSize)>len?len-i:blockSize;
	    for (j in 0...block)
	    {
		src.set(i + j, src.get(i + j) ^ vector.get(j) );
	    }
            vector = chiperText.sub(0,chiperText.length);
			
	    i += blockSize;
	}
    }

    public static function decrypt( src : Bytes, iv : Bytes, blockSize : Int, decryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
	encrypt(src,iv,blockSize,decryptBlock);
    }
}
