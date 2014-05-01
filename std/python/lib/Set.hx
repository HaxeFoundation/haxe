
package python.lib;

import python.NativeIterator;
import python.NativeIterable;

@:pythonImport("builtins", "set")
extern class Set <T>
{
	@:overload(function (?array:Array<T>):Void {})
	public function new (?iterable:NativeIterable<T>):Void;

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

	function __iter__ ():NativeIterator<T>;

	public inline function iterator ():NativeIterator<T>
	{
		return __iter__();
	}
}