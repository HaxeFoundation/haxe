package eval;

import haxe.io.Bytes;

@:coreType abstract NativeString {
	@:from static public function fromString(s:String):NativeString;
	@:from static public function fromBytes(b:Bytes):NativeString;
	@:to extern public function toString():String;
	@:to extern public function toBytes():Bytes;
	extern public function equals(b:NativeString):Bool;
}