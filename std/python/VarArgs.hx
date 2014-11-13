
package python;

import python.lib.Builtin;

@:analyzer(no_simplification)
abstract VarArgs (Dynamic)
{
	inline function new (d:Array<Dynamic>) this = d;


	inline function raw ():Dynamic {
		return this;
	}

	@:to public inline function toArray ():Array<Dynamic>
	{
		return if (!Std.is(raw(), Array)) Builtin.list(raw()) else (raw():Array<Dynamic>);
	}

	@:from static inline function fromArray (d:Array<Dynamic>):VarArgs
	{
		return new VarArgs(d);
	}
}
