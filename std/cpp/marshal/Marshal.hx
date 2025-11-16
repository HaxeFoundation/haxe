package cpp.marshal;

import cpp.Char;
import cpp.UInt8;
import cpp.Char16;

@:semantics(value)
@:cpp.ValueType({ namespace : [ 'cpp', 'marshal' ] })
final extern class Marshal {
	static overload function toCharView(s:String):View<Char>;
	static overload function toCharView(s:String, buffer:View<Char>):Int;
	static overload function toWideCharView(s:String):View<Char16>;
	static overload function toWideCharView(s:String, buffer:View<Char16>):Int;

	static function toString(buffer:View<Char>):String;
	static function toString(buffer:View<Char16>):String;

	static function read<T>(view:View<UInt8>):T;
	static function write<T>(view:View<UInt8>, value:T):Void;

	static function to<T>(view:View<UInt8>):T;
}