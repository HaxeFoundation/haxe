package haxe.crypto.mode;

import haxe.io.Bytes;

class PCBC
{
    public static function encrypt( src : Bytes, iv : Bytes, blockSize : Int, encryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
	var vector = iv.sub(0,iv.length);
	var i : Int = 0;
	var len : Int = src.length;
	while (i < len)
	{
		var plainText = src.sub(i,blockSize);
		for (j in 0...blockSize)
		{
			src.set(i + j, src.get(i + j) ^ vector.get(j) );
		}
		
		encryptBlock(src, i, src , i);
		vector = src.sub(i,blockSize);
		for (j in 0...blockSize)
		{
			vector.set(j, vector.get(j) ^ plainText.get(j) );
		}
		i += blockSize;
	}
    }

    public static function decrypt( src : Bytes, iv : Bytes, blockSize : Int, decryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
	var vector = iv.sub(0,iv.length);
	var i : Int = 0;
	var len : Int = src.length;
	while (i < len)
	{
		var cipherText = src.sub(i,blockSize);
		decryptBlock(src, i, src, i);
		for (j in 0 ... blockSize) 
			src.set(i + j, src.get(i + j) ^ vector.get(j));
		vector = src.sub(i,blockSize);
		for (j in 0...blockSize)
		{
			vector.set(j, vector.get(j) ^ cipherText.get(j) );
		}
		i += blockSize;
	}
    }
}
