
package python.lib;

import python.lib.Builtin;
import python.Syntax;

@:pythonImport("builtins", "tuple")
extern class Tuple<X> implements ArrayAccess<X> {

	@:overload(function ():Void {})
	public function new (a:Array<X>):Void;

	public var length(get_length, null):Int;

	inline function get_length():Int {
		return Builtin.len(this);
	}

	public inline function toArray ():Array<X>
	{
		return Builtin.list(this);
	}

}

extern class Tup2 <A,B> extends Tuple<Dynamic>
{
	public static inline function create <A,B>(a:A, b:B):Tup2<A,B> return Syntax.tuple(a,b);
	public var _1(get, null):A;
	public inline function get__1():A return this[0];
	public var _2(get, null):B;
	public inline function get__2():B return this[1];
}

extern class Tup3 <A,B,C> extends Tuple<Dynamic>
{
	public static inline function create <A,B,C>(a:A, b:B,c:C):Tup3<A,B,C> return Syntax.tuple(a,b,c);
	public var _1(get, null):A;
	public inline function get__1():A return this[0];
	public var _2(get, null):B;
	public inline function get__2():B return this[1];
	public var _3(get, null):C;
	public inline function get__3():C return this[2];
}

extern class Tup4 <A,B,C,D> extends Tuple<Dynamic>
{
	public static inline function create <A,B,C,D>(a:A, b:B,c:C,d:D):Tup4<A,B,C,D> return Syntax.tuple(a,b,c,d);
	public var _1(get, null):A;
	public inline function get__1():A return this[0];
	public var _2(get, null):B;
	public inline function get__2():B return this[1];
	public var _3(get, null):C;
	public inline function get__3():C return this[2];
	public var _4(get, null):D;
	public inline function get__4():D return this[3];
}

extern class Tup5 <A,B,C,D,E> extends Tuple<Dynamic>
{
	public static inline function create <A,B,C,D,E>(a:A, b:B,c:C,d:D,e:E):Tup5<A,B,C,D,E> return Syntax.tuple(a,b,c,d,e);
	public var _1(get, null):A;
	public inline function get__1():A return this[0];
	public var _2(get, null):B;
	public inline function get__2():B return this[1];
	public var _3(get, null):C;
	public inline function get__3():C return this[2];
	public var _4(get, null):D;
	public inline function get__4():D return this[3];
	public var _5(get, null):E;
	public inline function get__5():E return this[4];
}