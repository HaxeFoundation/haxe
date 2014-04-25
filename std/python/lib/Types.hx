
package python.lib;


import python.lib.Builtin;
import python.lib.io.IOBase;

abstract Choice <A,B>(Dynamic) {
	@:from public static inline function fromA <A,B>(x:A):Choice<A,B> return cast x;
	@:from public static inline function fromB <A,B>(x:B):Choice<A,B> return cast x;
}

typedef Variant<A,B> = Dynamic;
typedef Variant3<A,B,C> = Dynamic;
typedef Variant4<A,B,C,D> = Dynamic;







typedef NativeHashable = {
	public function __hash__():Int;
}

typedef NativeEqual = {
	public function __eq__(other:Dynamic):Int;
}

typedef NativeComparable = {

	public function __cmp__(other:Dynamic):Int;
}



extern class TB {}
extern class Frame {}
