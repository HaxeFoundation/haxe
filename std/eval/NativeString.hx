package eval;

import haxe.io.Bytes;

@:coreType abstract NativeString {
	@:from static public function fromString(s:String):NativeString;

	@:from static public function fromBytes(b:Bytes):NativeString;

	public function toString():String;

	public function toBytes():Bytes;

	@:op(A + B)
	public function concat(s:NativeString):NativeString;
}