package python.lib;

import python.lib.Builtin;
import python.Syntax;

extern class ByteArray implements ArrayAccess<Int> {
	public var length(get, null):Int;
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