package python;

import python.lib.Builtin.list;

/**
	This type represents python `*args` feature, supporting
	passing arbitrary number of arguments to a function.

	Example:

		function f(args:VarArgs<Int>) {}
		f([1, 2, 3]);
**/
@:analyzer(no_simplification)
abstract VarArgs<T>(Dynamic) {
	inline function new(d:Array<T>) {
		this = d;
	}

	inline function raw():Dynamic {
		return this;
	}

	@:to public inline function toArray():Array<T> {
		return if (!Std.is(raw(), Array)) list(raw()) else (raw() : Array<T>);
	}

	@:from static inline function fromArray<T>(d:Array<T>):VarArgs<T> {
		return new VarArgs(d);
	}
}
