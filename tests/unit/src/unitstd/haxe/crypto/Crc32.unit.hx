haxe.crypto.Crc32.make(haxe.io.Bytes.fromString("")) == 0;
haxe.crypto.Crc32.make(haxe.io.Bytes.fromString("The quick brown fox jumps over the lazy dog")) == 0x414FA339;