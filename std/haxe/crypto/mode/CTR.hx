package haxe.crypto.mode;

import haxe.io.Bytes;

class CTR
{
    public static function encrypt( src : Bytes, iv : Bytes, blockSize : Int, encryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
	var vector:Bytes;
        var vkey : Bytes = iv.sub(0,iv.length); 
        var i : Int = 0;
        var len : Int = src.length;
	while (i < len)
	{
            vector = vkey.sub(0,vkey.length);
            encryptBlock(vector, 0, vector , 0);
            
            var block:Int = (i+blockSize)>len?len-i:blockSize;
	    for (j in 0...block)
	    {
		src.set(i + j, src.get(i + j) ^ vector.get(j) );
	    }

            var j = blockSize;
            while (j-- >= 0) {
                vkey.set(j, vkey.get(j) + 1);
                if (vkey.get(j) != 0) break;  
            }

	    i += blockSize;
	}
    }

    public static function decrypt( src : Bytes, iv : Bytes, blockSize : Int, decryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
	encrypt(src,iv,blockSize,decryptBlock);
    }
}
