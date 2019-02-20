haxe.crypto.Crc32.make(haxe.io.Bytes.ofString("")) == 0;
#if !eval
haxe.crypto.Crc32.make(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog")) == 0x414FA339;
#end