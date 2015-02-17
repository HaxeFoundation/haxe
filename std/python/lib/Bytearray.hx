package python.lib;

import python.lib.Builtin;
import python.Syntax;

@:pythonImport("builtins", "bytearray")
extern class Bytearray implements ArrayAccess<Int> {

	public var length(get, null):Int;

	@:overload(function (it:Array<Int>):Void {})
	@:overload(function (it:NativeIterable<Int>):Void {})
	@:overload(function (size:Int):Void {})
	public function new (source:String,encoding:String,?errors:Dynamic):Void;

	public inline function get_length ():Int {
		return Builtin.len(this);
	}

	public inline function get(i:Int):Int {
		return Syntax.arrayAccess(this, i);
	}

	public inline function set(i:Int,v:Int):Void {
        this.__setitem__(i,v);
    }

    public function __setitem__(i:Int,v:Int):Void;

	public function decode(encoding:String="utf-8", errors:String="strict"):String;
}