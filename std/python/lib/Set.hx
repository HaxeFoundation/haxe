
package python.lib;

import python.lib.Types.PyIterator;

extern class Set <T>
{
	@:overload(function (?array:Array<T>):Void {})
	public function new (?iterable:python.lib.Types.PyIterable<T>):Void;

	public inline function length ():Int
	{
		return python.lib.Builtin.len(this);
	}

	public inline function has (v:T):Bool
	{
		return python.Syntax.isIn(v, this);
	}


	public inline function minus (other:Set<T>):Set<T>
	{
		return python.Syntax.binop(this, "-", other);
	}
	public inline function plus (other:Set<T>):Set<T>
	{
		return python.Syntax.binop(this, "+", other);
	}

	static function __init__ ():Void
	{
		Syntax.importFromAs("builtins", "set", "python.lib.Set");
	}

	function __iter__ ():PyIterator<T>;

	public inline function iterator ():Iterator<T>
	{
		return __iter__();
	}
}