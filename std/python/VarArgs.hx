
package python;

abstract VarArgs (Array<Dynamic>)
{
	inline function new (d:Array<Dynamic>) this = d;

	@:to inline function toArray ():Array<Dynamic>
	{
		return this;
	}
	@:from static inline function fromArray (d:Array<Dynamic>):VarArgs
	{
		return new VarArgs(d);
	}
}
