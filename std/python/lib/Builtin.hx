
package python.lib;


import python.lib.io.FileIO;
import python.lib.Dict;
import python.NativeIterable;
import python.NativeIterator;

@:pythonImport("builtins")
extern class Builtin {



	@:overload(function (f:Int):Int {})
	public static function abs(x:Float):Float;
	public static function all(i:Iterable<Bool>):Bool;
	public static function any(i:Iterable<Bool>):Bool;

	public static function bool(x:Dynamic):Bool;

	public static function issubclass(x:Class<Dynamic>, from:Class<Dynamic>):Bool;
	public static function callable(x:Dynamic):Bool;




	@:overload(function(obj:Dynamic, f:Tuple<Dynamic>):Bool {})
	public static function isinstance(obj:Dynamic, cl:Dynamic):Bool;

	public static function hasattr(obj:Dynamic, attr:String):Bool;
	public static function getattr(obj:Dynamic, attr:String):Dynamic;

	@:overload(function (f:Set<Dynamic>):Int {})
	@:overload(function (f:StringBuf):Int {})
	@:overload(function (f:Array<Dynamic>):Int {})
	@:overload(function (f:Dict<Dynamic, Dynamic>):Int {})
	@:overload(function (f:Bytes):Int {})
	@:overload(function (f:DictView<Dynamic>):Int {})
	@:overload(function (f:ByteArray):Int {})
	@:overload(function (f:Tuple<Dynamic>):Int {})
	public static function len(x:String):Int;

	public static function open(file:String, mode:String, ?buffering:Int = -1, ?encoding:String = null, ?errors : String, ?newline:String, ?closefd:Bool, ?opener:String->Int->FileDescriptor):FileIO;

	//public static function divmod():Void;
	//public static function input():Void;

	//public static function staticmethod():Void;
	//public static function enumerate():Void;
	@:overload(function (x:Dynamic, base:Int):Int {})
	public static function int(x:Dynamic):Int;
	public static function ord(s:String):Int;
	public static function str(o:Dynamic):String;
	//public static function eval():Void;

	//public static function pow():Void;
	//public static function sum():Void;
	//public static function basestring():Void;
	//public static function execfile():Void;

	public static inline function print(o:Dynamic):Void {
		python.Syntax.field(Builtin, "print")(o);
	}

	//public static function super():Void;
	//public static function bin():Void;
	//public static function file():Void;
	public static function iter<X>(d:DictView<X>):NativeIterator<X>;
	//public static function property():Void;



	@:overload(function <X>():Tuple<X> {})
	public static function tuple<X>(a:Array<X>):Tuple<X>;




	//public static function range():Void;

	public static function type():Void;
	@:overload(function (it:Array<Int>):python.lib.ByteArray {})
	@:overload(function (it:NativeIterable<Int>):python.lib.ByteArray {})
	@:overload(function (size:Int):python.lib.ByteArray {})
	public static function bytearray(source:String,encoding:String,?errors:Dynamic):python.lib.ByteArray;
	public static function float(x:Dynamic):Float;

	@:overload(function <T>(f:Array<T>):Array<T> {})
	@:overload(function <T>(f:Tuple<T>):Array<T> {})
	@:overload(function <T>(f:Dict.DictView<T>):Array<T> {})
	@:overload(function (f:String):Array<String> {})
	public static function list<T>(i:NativeIterable<T>):Array<T>;

	@:overload(function <A>(f:A->Bool, i:NativeIterable<A>):NativeIterator<A> {})
	public static function filter<A>(f:A->Bool, i:Array<A>):NativeIterator<A>;
	//public static function raw_input():Void;
	//public static function unichr():Void;

	//public static function format():Void;
	//public static function locals():Void;
	//public static function reduce():Void;
	//public static function unicode():Void;
	public static function chr(c:Int):String;
	//public static function frozenset():Void;
	//public static function long():Void;
	//public static function reload():Void;
	//public static function vars():Void;
	//public static function classmethod():Void;

	public static function map<A,B>(fn:A->B, it:NativeIterable<A>):NativeIterator<B>;
	//public static function repr():Void;
	//public static function xrange():Void;
	//public static function cmp():Void;
	//public static function globals():Void;
	@:overload(function (a1:Float, a2:Float, rest:haxe.Rest<Float>):Float {})
	public static function max(a1:Int, a2:Int, rest:haxe.Rest<Int>):Int;
	//public static function reversed():Void;
	//public static function zip():Void;
	//public static function compile():Void;

	//public static function memoryview():Void;
	public static function round(f:Float):Int;
	//public static function __import__():Void;
	//public static function complex():Void;
	//public static function hash():Void;
	@:overload(function (a1:Float, a2:Float, rest:haxe.Rest<Float>):Float {})
	public static function min(a1:Int, a2:Int, rest:haxe.Rest<Int>):Int;
	//public static function set():Void;
	//public static function apply():Void;
	public static function delattr(o:Dynamic, attr:String):Void;
	//public static function help():Void;
	//public static function next():Void;
	public static function setattr(o:Dynamic, attr:String, val:Dynamic):Void;
	//public static function buffer():Void;
	//public static function dict():Void;
	//public static function hex():Void;
	//public static function object():Void;
	//public static function slice():Void;
	//public static function coerce():Void;
	//public static function dir():Void;
	public static function id(x:{}):Int;
	//public static function oct():Void;
	//public static function sorted():Void;
	//public static function intern():Void;


}