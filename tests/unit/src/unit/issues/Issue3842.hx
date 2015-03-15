package unit.issues;

class Issue3842 extends Test
{
#if (cs || java)
	// public function test()
	// {
	// 	var c = new Child();
	// 	eq(c.test1, 42);
	// 	eq(c.test2, 4.2);
	// 	eq( (c.test3 = c), c );
	// }
#end
}
//
// #if (cs || java)
// private class Base
// {
// 	public function new()
// 	{
// 	}
//
// 	@:overload function get_test1(i:Int):Int
// 	{
// 		return i;
// 	}
//
// 	@:overload function get_test1():Int
// 	{
// 		return 42;
// 	}
//
// 	@:overload function get_test2(f:Float):Float
// 	{
// 		return f;
// 	}
// }
//
// private class Child extends Base
// {
// 	public var test1(get,null):Int;
// 	public var test2(get,null):Float;
// 	public var test3(null,set):Base;
//
// 	@:overload function get_test2():Float
// 	{
// 		return 4.2;
// 	}
//
// 	@:overload function set_test3(b:Base):Base
// 	{
// 		return b;
// 	}
//
// 	@:overload function set_test3(b:Base,i:Int):Base
// 	{
// 		return null;
// 	}
//
// 	@:overload function set_test3(i:Int):Base
// 	{
// 		return null;
// 	}
// }
//
// #end
