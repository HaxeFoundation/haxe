package java.lang;
import haxe.Int64;
import java.StdTypes;

private typedef StdFloat = Float;

@:abstract extern class Number
{

	public function byteValue():Int8;
	public function doubleValue():StdFloat;
	public function floatValue():Single;
	public function intValue():Int;
	public function longValue():Int64;
	public function shortValue():Int16;

}

@:final extern class Byte extends Number/*, implements Int */
{
	static var MAX_VALUE(default, null):Int8;
	static var MIN_VALUE(default, null):Int8;

	@:overload(function(s:String):Void {})
	function new(value:Int8):Void;

	static function parseByte(s:String, radix:Int):Int8;
}

@:hack @:final extern class Double extends Number/* , implements StdFloat */
{
	static var MAX_VALUE(default, null):StdFloat;
	static var MIN_VALUE(default, null):StdFloat;
	static var NaN(default, null):StdFloat;
	static var NEGATIVE_INFINITY(default, null):StdFloat;
	static var POSITIVE_INFINITY(default, null):StdFloat;

	@:overload(function(s:String):Void {})
	function new(value:StdFloat):Void;

	public static function isInfinite(f:Float):Bool;
	public static function isNaN(f:Float):Bool;
}

@:final extern class Float extends Number /*, implements StdFloat*/
{
	static var MAX_VALUE(default, null):Single;
	static var MIN_VALUE(default, null):Single;
	static var NaN(default, null):Single;
	static var NEGATIVE_INFINITY(default, null):Single;
	static var POSITIVE_INFINITY(default, null):Single;

	static function isNaN(f:Float):Bool;

	@:overload(function(s:String):Void {})
	function new(value:Single):Void;
}

@:final extern class Integer extends Number/*, implements Int */
{
	static var MAX_VALUE(default, null):Int;
	static var MIN_VALUE(default, null):Int;

	@:overload(function(s:String):Void {})
	function new(value:Int):Void;

	static function toString(i:Int):String;
	static function parseInt(s:String, radix:Int):Int;
}

@:final extern class Long extends Number
{
	static var MAX_VALUE(default, null):Int64;
	static var MIN_VALUE(default, null):Int64;

	@:overload(function(s:String):Void {})
	function new(value:Int64):Void;

	static function toString(i:Int64):String;
	static function parseLong(s:String, radix:Int):Int64;
}

@:final extern class Short extends Number/*, implements Int */
{
	static var MAX_VALUE(default, null):Int16;
	static var MIN_VALUE(default, null):Int16;

	@:overload(function(s:String):Void {})
	function new(value:Int16):Void;

	static function parseShort(s:String, radix:Int):Int16;
}

