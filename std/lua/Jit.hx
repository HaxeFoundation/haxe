package lua;

import haxe.Constraints.Function;

#if lua_jit
@:native("_G.jit")
extern class Jit {
	static function on(?f:Function, ?recursive:Bool):Void;
	static function off(?f:Function, ?recursive:Bool):Void;
	static function flush(?f:Function, ?recursive:Bool):Void;
	static function status():Bool;
	static var version:String;
	static var version_num:Int;
	static var os:String;
	static var arch:String;
	static var opt:{start:Function};
	static var util:Dynamic;
}
#end
