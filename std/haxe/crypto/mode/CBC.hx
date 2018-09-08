package haxe.crypto.mode;

import haxe.io.Bytes;

class CBC 
{
    public static function encrypt( src : Bytes, iv : Bytes, blockSize : Int, encryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
	var vector = iv.sub(0,iv.length);
	var i : Int = 0;
.	var len : Int = src.length;
	while (i < len)
	{
		for (j in 0...blockSize)
		{
			src.set(i + j, src.get(i + j) ^ vector.get(j) );
		}
		encryptBlock(src, i, src , i);

		vector = src.sub(i,blockSize);
		i += blockSize;
	}
    }

    public static function decrypt( src : Bytes, iv : Bytes, blockSize : Int, decryptBlock : Bytes->Int->Bytes->Int->Void) : Void
    {
        var vpos : Int = src.length - blockSize;
        var i : Int = src.length;
        while(i > 0 )
        {
            i -= blockSize;
            vpos -= blockSize;
			
            decryptBlock(src, i, src, i);

            if ( vpos < 0 ) {
                for (j in 0 ... blockSize) 
			    	src.set(j, src.get(j) ^ iv.get(j));
            } else {
                for (j in 0 ... blockSize) 
			    	src.set(i + j, src.get(i + j) ^ src.get(vpos + j));
            }
        }
    }

}
