package haxe.crypto;

import haxe.io.Bytes;
import haxe.crypto.mode.*;
import haxe.crypto.padding.*;

class TripleDes extends Des
{
    static inline var BLOCK_SIZE : Int = 8;

    private var keys:Array<Array<Int64>>;

    public var iv(default, set):Bytes;

    function set_iv(vector) {
        iv = vector;
        if (iv == null) 
        {
            iv = Bytes.alloc(BLOCK_SIZE);
            iv.fill(0,BLOCK_SIZE,0x00);
        }
        
        return iv;
    }

	public function new(?key:Bytes, ?iv:Bytes)
    {
        super(key,iv);
        if ( key != null ) init(key,iv);         // Key size 168 bits
        keys = new Array<Array<Int64>>();
    }

    public function init(key:Bytes, ?iv:Bytes):Void
    {
		var keyLength = key.length;
        if ( keyLength < 24 ) throw "Specified key is not a valid size for 3Des algorithm";
		
        this.iv = iv;
        keys[0] = keygen(key);
        keys[1] = keygen(key,8);
        keys[2] = keygen(key,16);
    }

    public function encrypt(cipherMode:Mode, data:Bytes, ?padding:Padding=Padding.PKCS5):Bytes
    { 
        var out:Bytes;
        
        switch(padding)  {
            //CBC, ECB  and PCBC requires padding
            case Padding.NoPadding:
                out = NoPadding.pad(data,BLOCK_SIZE); 
            case Padding.PKCS5:
                out = PKCS5.pad(data,BLOCK_SIZE);
            case Padding.BitPadding:
                out = BitPadding.pad(data,BLOCK_SIZE);
            case Padding.AnsiX923:
                out = AnsiX923.pad(data,BLOCK_SIZE);
            case Padding.ISO10126:
                out = ISO10126.pad(data,BLOCK_SIZE);
            case Padding.NullPadding:
                out = NullPadding.pad(data,BLOCK_SIZE);
            case Padding.SpacePadding:
                out = SpacePadding.pad(data,BLOCK_SIZE);
            case Padding.TBC:
                out = TBC.pad(data,BLOCK_SIZE);
        }

        switch (cipherMode) {
            case Mode.CBC:
                CBC.encrypt(out,iv,BLOCK_SIZE,encryptBlock);
            case Mode.ECB:
                ECB.encrypt(out,BLOCK_SIZE,encryptBlock);
            case Mode.PCBC:
                PCBC.encrypt(out,iv,BLOCK_SIZE,encryptBlock);
            case Mode.CTR:
                CTR.encrypt(out,iv,BLOCK_SIZE,encryptBlock);
            case Mode.CFB:
                CFB.encrypt(out,iv,BLOCK_SIZE,encryptBlock);
            case Mode.OFB:
                OFB.encrypt(out,iv,BLOCK_SIZE,encryptBlock);
        }

        return out;
    }

    public function decrypt(cipherMode:Mode, data:Bytes, ?padding:Padding=Padding.PKCS5):Bytes 
    {
        var out:Bytes = data;

        switch (cipherMode) {
            case Mode.CBC:
                CBC.decrypt(out,iv,BLOCK_SIZE,decryptBlock);
            case Mode.ECB:
                ECB.decrypt(out,BLOCK_SIZE,decryptBlock);
            case Mode.PCBC:
                PCBC.decrypt(out,iv,BLOCK_SIZE,decryptBlock);
            case Mode.CTR:
                CTR.decrypt(out,iv,BLOCK_SIZE,encryptBlock);
            case Mode.CFB:
               CFB.decrypt(out,iv,BLOCK_SIZE,encryptBlock);
            case Mode.OFB:
                OFB.decrypt(out,iv,BLOCK_SIZE,encryptBlock);
        }

        switch(padding)  {
            case Padding.NoPadding:
                out = NoPadding.unpad(out);
            case Padding.PKCS5:
                out = PKCS5.unpad(out);
            case Padding.BitPadding:
                out = BitPadding.unpad(out);
            case Padding.AnsiX923:
                out = AnsiX923.unpad(out);
            case Padding.ISO10126:
                out = ISO10126.unpad(out);
            case Padding.NullPadding:
                out = NullPadding.unpad(out);
            case Padding.SpacePadding:
                out = SpacePadding.unpad(out);
            case Padding.TBC:
                out = TBC.unpad(out);
        }

        return out;
    }

    private function enc(block:Int64, mode:Bool, idx:Int):Int64
    {
       return cipher(block, mode, keys[idx]); 
    }

    private function encryptBlock( src:Bytes, srcIndex:Int, dst:Bytes, dstIndex:Int):Void
    {
        var block = bytesToInt64(src, srcIndex);
        var xb =  enc(enc(enc(block,true,0),false,1),true,2);
        int64ToBytes(xb, dst, dstIndex);
    }

    private function decryptBlock( src:Bytes, srcIndex:Int, dst:Bytes, dstIndex:Int):Void
    {
        var block = bytesToInt64(src, srcIndex);
        var xb =  enc(enc(enc(block,false,2),true,1),false,0);
        int64ToBytes(xb, dst, dstIndex);
    }

}