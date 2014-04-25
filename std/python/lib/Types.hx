
package python.lib;


import python.lib.Builtin;
import python.lib.io.IOBase;

abstract Choice <A,B>(Dynamic) {
	@:from public static inline function fromA <A,B>(x:A):Choice<A,B> return cast x;
	@:from public static inline function fromB <A,B>(x:B):Choice<A,B> return cast x;
}








typedef NativeHashable = {
	public function __hash__():Int;
}

typedef NativeEqual = {
	public function __eq__(other:Dynamic):Int;
}

typedef NativeComparable = {

	public function __cmp__(other:Dynamic):Int;
}




