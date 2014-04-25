
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

abstract PyIterator <T>(NativeIterator<T>) to NativeIterator<T> to PyIterable<T> {
	public inline function new (p:NativeIterator<T>) this = p;
	@:to public static inline function toHaxeIterator <T>(p:NativeIterator<T>):HaxeIterator<T> return new HaxeIterator(p);
	@:to public static inline function toPyIterable <T>(p:NativeIterator<T>):PyIterable<T> return p;
	public function getNativeIterator <T>():NativeIterator<T> return this;
}

abstract PyIterable <T>(NativeIterable<T>) to NativeIterable<T> from NativeIterable<T> {
	@:to public static inline function toHaxeIterable <T>(p:NativeIterable<T>):HaxeIterable<T> return new HaxeIterable(p);

	//@:from public static inline function fromArray <T>(p:Array<T>):PyIterable<T> return cast p;

	public inline function iterator <T>() return IterHelper.iterableToIterator(this);
	public function getNativeIterable <T>():NativeIterable<T> return this;
	public function getNativeIterator <T>():NativeIterator<T> return this.__iter__();

}

class IterHelper {
	public static inline function iterableToIterator <T>(it:PyIterable<T>):Iterator<T>
	{
		return it.toHaxeIterable().iterator();
	}
}

typedef NativeIterator<T> = {
	function __next__ ():T;
	function __iter__ ():PyIterator<T>;
}

typedef NativeIterable<T> = {
	function __iter__():PyIterator<T>;
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

typedef FileObject = IOBase;

extern class FileDescriptor {

}


//typedef DictKey<T> = {
//	function __hash__():Int;
//	function __eq__(other:Dynamic):Int;
//	function __cmp__(other:Dynamic):Int;
//}

//@:native("set")








extern class TB {}
extern class Frame {}
