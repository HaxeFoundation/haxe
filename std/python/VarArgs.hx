package python;

import python.lib.Builtin;

@:analyzer(no_simplification)
abstract VarArgs<T>(Dynamic) {
	inline function new(d:Array<T>) {
        this = d;
    }

	inline function raw():Dynamic {
        return this;
    }

	@:to public inline function toArray():Array<T> {
		return if (!Std.is(raw(), Array)) Builtin.list(raw()) else (raw() : Array<T>);
	}

	@:from static inline function fromArray<T>(d:Array<T>):VarArgs<T> {
		return new VarArgs(d);
	}
}
