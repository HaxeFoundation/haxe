package eval;

import haxe.io.Bytes;

@:coreType abstract NativeString {
	/** String length */
	public var length(get,never):Int;
	function get_length():Int;

	@:from static public function fromString(s:String):NativeString;

	@:from static public function fromBytes(b:Bytes):NativeString;

	/**
		Returns a character at the specified `index`.

		Throws an exception if `index` is outside of the string bounds.
	**/
	public function char(index:Int):String;

	/**
		Returns a character code at the specified `index`.

		Throws an exception if `index` is outside of the string bounds.
	**/
	public function code(index:Int):Int;

	/**
		Returns a fresh string up to `length` characters long, containing the
		substring that starts at position `start`.

		If `length` is not specified the all characters from `start` to the end
		of this string are returned.

		Throws an exception if `index` is outside of the string bounds.
	**/
	public function sub(start:Int, ?length:Int):NativeString;

	public function toString():String;

	public function toBytes():Bytes;

	@:op(A + B)
	public function concat(s:NativeString):NativeString;
}