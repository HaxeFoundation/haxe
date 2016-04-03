package lua;

import haxe.Constraints.Function;

#if lua_jit
@:native("_G.jit")
extern class Jit {
	public static function on(?f:Function, ?recursive:Bool) : Void;
	public static function off(?f:Function, ?recursive:Bool) : Void;
	public static function flush(?f:Function, ?recursive:Bool) : Void;
	public static function status() : Bool;
	public static var version : String;
	public static var version_num : Int;
	public static var os : String;
	public static var arch : String;
	public static var opt : { start : Function };
	public static var util : Dynamic;
}
#end


