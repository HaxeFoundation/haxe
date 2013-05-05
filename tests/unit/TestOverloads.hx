package unit;
import haxe.io.Bytes;
import unit.TestType.typeError;
import unit.TestType.typedAs;
using unit.TestOverloads.UsingTest2;
using unit.TestOverloads.UsingTest3;

class TestOverloads extends Test
{

	public function testAmbiguous()
	{
		var amb = new Ambiguous();
		//both Int,Float as Float,Int should be considered here
		t(typeError(amb.amb1(1,1)));
		t(typeError(Ambiguous.amb2(1,1)));
		var dyn:Dynamic = null;
		eq(amb.amb1(1,1.0), "Int,Float");
		//TODO: typedAs version that can handle overloads (possible?)
		// typedAs(amb.amb1(1,1.0), function(i:Int, f:Float):String return null);
		eq(amb.amb1(1.0,1), "Float,Int");
		// typedAs(amb.amb1(1,1.0), function(i:Float, f:Int):String return null);
		eq(Ambiguous.amb2(1,1.0), "Int,Float");
		// typedAs(Ambiguous.amb2(1,1.0), function(i:Int, f:Float):String return null);
		eq(Ambiguous.amb2(1.0,1), "Float,Int");
		// typedAs(Ambiguous.amb2(1,1.0), function(i:Float, f:Int):String return null);
		//fewer unused optional arguments have precedence
		eq(amb.amb1(dyn,dyn), "Int,Dynamic");
		// typedAs(amb.amb1(dyn,dyn), function(i:Int, f:Dynamic):String return null);
		eq(amb.amb1(dyn,1), "Float,Int");
		// typedAs(amb.amb1(dyn,1), function(i:Float, f:Int):String return null);
		//Null<Int> has precedence over Int when passing a Dynamic type
		eq(amb.amb1(dyn,dyn,null), "Null<Int>,Dynamic,Bool");
		// typedAs(amb.amb1(dyn,dyn,null), function(i:Null<Int>, f:Dynamic, ?b:Bool):String return null);
		eq(amb.amb1(dyn,1,null), "Null<Int>,Dynamic,Bool");
		// typedAs(amb.amb1(dyn,1,null), function(i:Null<Int>, f:Dynamic, ?b:Bool):String return null);
		eq(amb.amb1(1,{}), "Int,Dynamic");
		// typedAs(amb.amb1(1,{}), function(i:Int, f:Dynamic):String return null);

		var amb = new AmbiguousChild();
		t(typeError(amb.amb1(1,1)));
		t(typeError(Ambiguous.amb2(1,1)));
		eq(amb.amb1(1,1.0), "Int,Float");
		eq(amb.amb1(1.0,1), "Float,Int");
		eq(Ambiguous.amb2(1,1.0), "Int,Float");
		eq(Ambiguous.amb2(1.0,1), "Float,Int");
		eq(amb.amb1(dyn,dyn), "Int,Dynamic-2");
		eq(amb.amb1(dyn,1), "Float,Int");
		eq(amb.amb1(dyn,dyn,null), "Null<Int>,Dynamic,Bool");
		eq(amb.amb1(dyn,1,null), "Null<Int>,Dynamic,Bool");
		eq(amb.amb1(1,{}), "Int,Dynamic-2");
		eq(amb.amb1(1,true), "Int,Bool-2");
		var nb:Null<Bool> = null;
		eq(amb.amb1(1,nb), "Int,Bool-2");

		//make sure selections are virtual
		var amb:Ambiguous = amb;
		eq(amb.amb1(dyn,dyn), "Int,Dynamic-2");
		eq(amb.amb1(1,{}), "Int,Dynamic-2");
	}

	public function testUsing()
	{
		var t = new UsingTest1();
		eq(t.m1(1,1), "Int,Float"); //using must not influence fields that already exist
		var dyn:Dynamic = false;
		eq(t.m1(dyn), "Bool");
		eq(UsingTest1.m2(true), "Dynamic");

		//using won't be influenced by @:overload or @:overload resolution
		eq(t.m3(1.0,1), "Float,Int");
		// t(typeError(t.m3(1,1.0))); //typeError doesn't work with using statements
		// t(typeError(t.m3(dyn)));

		eq(t.m4(1.0,1), "Float,Int");
		// t(typeError(t.m4(1,1.0)));
		// t(typeError(t.m3(dyn)));
	}

	public function testInterface()
	{
		var it:InterfaceTest = new InterfaceTest();
		var dyn:Dynamic = null;
		var ni:Null<Int> = null;

		eq(it.m(1), "Int");
		eq(it.m(ni), "Int");
		eq(it.m(dyn), "Dynamic");

		var i2:I2 = it;
		eq(i2.m(1), "Int");
		eq(i2.m(ni), "Int");
		eq(i2.m(dyn), "Dynamic");

		var i1:I1 = i2;
		eq(i1.m(1), "Dynamic");
		eq(i1.m(ni), "Dynamic");
		eq(i1.m(dyn), "Dynamic");
	}

	public function testGenerics1()
	{
		var itest = new InterfaceTest();
		var i0:I0 = itest, i1:I1 = itest, i2:I2 = itest;
		var d = new D(itest);
		var dyn:Dynamic = null;
		eq(d.foo(i0), "I0");
		eq(d.foo(itest), "InterfaceTest");
		eq(d.bar(itest), "T");

		var c:C = d;
		eq(c.foo(i0), "I0");
		eq(c.foo(itest), "InterfaceTest");
		eq(c.bar(itest), "T");

		var b:B<InterfaceTest> = c;
		eq(b.foo(itest), "InterfaceTest");
		eq(b.bar(itest), "T");
		t(typeError(b.foo(i1)));
		t(typeError(b.foo(i2)));

		var a:A<InterfaceTest> = b;
		eq(a.foo(itest), "InterfaceTest");
		t(typeError(a.foo(i1)));
		t(typeError(a.foo(i2)));

		var b = new B(i2);
		eq(b.foo(itest), "T-2");
		eq(b.foo(i2), "T-2");
		t(typeError(b.foo(i1)));

		var a = b;
		eq(a.foo(itest), "T-2");
		eq(a.foo(i2), "T-2");
		t(typeError(a.foo(i1)));

		var a = new A(itest);
		eq(a.foo(itest), "T");
		t(typeError(a.foo(i1)));
		t(typeError(a.foo(i2)));
	}

	public function testPrimitives()
	{
		eq(Primitives.prim(1), "Int");
		eq(Primitives.prim(1.0), "Float");
		var ni:Null<Int> = null;
		eq(Primitives.prim(ni, null), "Null<Int>");
		var nf:Null<Float> = null;
		eq(Primitives.prim(nf, null), "Null<Float>");
		var dyn:Dynamic = null;
		eq(Primitives.prim(dyn), "Dynamic");
#if (java || cs)
		var s:Single = 1.0;
		eq(Primitives.prim(s), "Single" );
		var ns:Null<Single> = null;
		eq(Primitives.prim(ns, null), "Null<Single>");
#end
	}

	//former java tests. only exact types
	public function testOverload()
	{
		var base = new BaseJava(1);
		eq(base.i, 1);
		eq(new BaseJava("test").s, "test");
		eq(base.someField(Bytes.ofString("test")), 0);
		eq(base.someField(0), 1);
		eq(base.someField("test"), 2);
		eq(base.someField(true), -1);

		eq(new ChildJava(4).i, 5);
		var child = new ChildJava(Bytes.ofString("a"));
		eq(child.s, "a");
		eq(child.someField("test") , 2);
		eq(child.someField(Bytes.ofString("a")), 2);
		eq(child.someField(22.2), 3);
		eq(new ChildJava(25).i, 26);
		eq(child.initialized, 10);
		eq(new ChildJava(100).initialized, 10);

		var child:OverloadedInterface = child;
		eq(child.someField("test"), 2);
		eq(child.someField(22.2), 3);
		eq(child.someField(true), -1);

		var child:NormalInterface = child;
		eq(child.someField(true), -1);

		var child:ChildJava2<ChildJava2<Dynamic>> = new ChildJava2(22.5);
		eq(child.i, 23);
		eq(child.someField(22.5), 50);
		eq(child.someField(child), child);
		eq(child.someField(ChildJava2), 51);
		eq(child.someField(true), -1);
		eq(child.initialized2, "20");

		var child:ChildJava3<Bool, BaseJava> = new ChildJava3(Bytes.ofString("test"));
		eq(child.s, "test");
		eq(child.someField(base), null);
		eq(child.someField(true, child, 99), 99);
		eq(child.someField(true, 10), 52);
		eq(child.initialized3, true);

		var child:ChildJava4<Int, Bool, ChildJava3<Dynamic, Dynamic>> = new ChildJava4(Bytes.ofString("test"));
		eq(child.s, "test");
		eq(child.someField(child), null);
	}
}

private class Primitives
{
	@:overload public static function prim(v:Dynamic):String
	{
		return "Dynamic";
	}

	@:overload public static function prim(v:Dynamic, ?nothing:Dynamic):String
	{
		return "Dynamic,?";
	}

	@:overload public static function prim(v:Null<Float>, ?nothing:String):String
	{
		return "Null<Float>";
	}

	@:overload public static function prim(v:Float, ?nothing:Dynamic):String
	{
		return "Float,?";
	}

	@:overload public static function prim(v:Float):String
	{
		return "Float";
	}

	@:overload public static function prim(v:Int):String
	{
		return "Int";
	}

	@:overload public static function prim(v:Int, ?nothing:Dynamic):String
	{
		return "Int,?";
	}

	@:overload public static function prim(v:Null<Int>, ?nothing:haxe.io.Bytes):String
	{
		return "Null<Int>";
	}

#if (java || cs)
	@:overload public static function prim(v:Single):String
	{
		return "Single";
	}

	@:overload public static function prim(v:Single, ?nothing:haxe.io.Bytes):String
	{
		return "Single,?";
	}

	@:overload public static function prim(v:Null<Single>, ?nothing:I0):String
	{
		return "Null<Single>";
	}
#end
}

private interface I0
{
}

private interface I1 extends I0
{
	@:overload function m(obj:Dynamic):String;
}

private interface I2 extends I1
{
	@:overload function m(i:Int):String;
}

private class InterfaceTest implements I2
{
	public function new()
	{

	}

	@:overload public function m(obj:Dynamic):String
	{
		return "Dynamic";
	}

	@:overload public function m(i:Int):String
	{
		return "Int";
	}
}

private class Ambiguous
{
	public function new() {

	}

	@:overload public function amb1(i:Int, f:Float):String
	{
		return "Int,Float";
	}

	@:overload public function amb1(f:Float, i:Int):String
	{
		return "Float,Int";
	}

	@:overload public function amb1(i:Int, d:Dynamic):String
	{
		return "Int,Dynamic";
	}

	@:overload public function amb1(i:Null<Int>, d:Dynamic, ?a:Bool):String
	{
		return "Null<Int>,Dynamic,Bool";
	}

	@:overload public function amb1(i:Int, d:Dynamic, ?a:String):String
	{
		return "Int,Dynamic,String";
	}

	@:overload public static function amb2(i:Int, f:Float):String
	{
		return "Int,Float";
	}

	@:overload public static function amb2(f:Float, i:Int):String
	{
		return "Float,Int";
	}

	@:overload public static function amb2(i:Int, d:Dynamic):String
	{
		return "Int,Dynamic";
	}
}

private class AmbiguousChild extends Ambiguous
{
	@:overload override public function amb1(i:Int, d:Dynamic):String
	{
		return "Int,Dynamic-2";
	}

	@:overload public function amb1(i:Int, b:Bool):String
	{
		return "Int,Bool-2";
	}
}

class UsingTest1
{
	public function new()
	{

	}

	@:overload public function m1(i:Int, f:Float):String
	{
		return "Int,Float";
	}

	@:overload public function m1(b:Bool):String
	{
		return "Bool";
	}

	@:overload public static function m2(d:Dynamic):String
	{
		return "Dynamic";
	}
}

class UsingTest2
{
	@:overload public static function m1(me:UsingTest1, f:Float, i:Int):String
	{
		return "Float,Int";
	}

	@:overload public static function m1(me:UsingTest1, d:Dynamic):String
	{
		return "Dynamic";
	}

	@:overload public static function m2(me:Class<UsingTest1>, b:Bool):String
	{
		return "Bool";
	}

	@:overload public static function m3(me:UsingTest1, f:Float, i:Int):String
	{
		return "Float,Int";
	}

	@:overload public static function m3(me:UsingTest1, i:Int, f:Float):String
	{
		return "Int,Float";
	}

	@:overload public static function m3(me:UsingTest1, b:Bool):String
	{
		return "Bool";
	}

	@:overload public static function m4(me:UsingTest1, i:Int, f:Float):String
	{
		return "Int,Float";
	}
}

class UsingTest3
{
	@:overload public static function m4(me:UsingTest1, f:Float, i:Int):String
	{
		return "Float,Int";
	}
}

private class A<T>
{
	public function new(a:T)
	{

	}

	@:overload public function foo(t:T):String
	{
		return "T";
	}

	@:overload public function foo(t:String):String
	{
		return "String";
	}

	@:overload public function bar(t:T):String
	{
		return "T";
	}
}

private class B<T : I1> extends A<T>
{
	@:overload override public function foo(t:T):String
	{
		return "T-2";
	}
}

private class C extends B<InterfaceTest>
{
	@:overload override public function foo(t:InterfaceTest):String
	{
		return "InterfaceTest";
	}

	@:overload public function foo(t:I0):String
	{
		return "I0";
	}

	@:overload public function bar(notChosen:I0):String
	{
		return "I0";
	}

	@:overload public function bar(unrelated:String):String
	{
		return "String";
	}
}

private class D extends C {}

//java tests
class BaseJava implements NormalInterface
{
	public var i:Int;
	public var s:String;
	public var f:Float;

	@:overload public function new(i:Int):Void
	{
		this.i = i;
	}

	@:overload public function new(s:String):Void
	{
		this.s = s;
	}

	@:overload public function new(f:Float):Void
	{
		this.f = f;
	}

	@:overload public function someField(b:haxe.io.Bytes):Int
	{
		return 0;
	}

	@:overload public function someField(i:Int):Int
	{
		return 1;
	}

	@:overload public function someField(s:String):Int
	{
		return 2;
	}

	@:overload public function someField(s:Bool):Int
	{
		return -1;
	}
}

class ChildJava extends BaseJava implements OverloadedInterface
{
	public var initialized = 10;

	@:overload public function new(b:haxe.io.Bytes)
	{
		super(b.toString());
	}

	@:overload public function new(i:Int)
	{
		super(i + 1);
	}

	@:overload public function someField(f:Float):Int
	{
		return 3;
	}

	@:overload override public function someField(b:haxe.io.Bytes)
	{
		return 2;
	}
}

class ChildJava2<T> extends ChildJava
{
	public var initialized2 = "20";

	@:overload public function new(x:Float)
	{
		super(Std.int(x));
	}
	@:overload private function new(b:haxe.io.Bytes)
	{
		super(b);
	}

	@:overload override public function someField(f:Float):Int
	{
		return 50;
	}

	@:overload public function someField(t:T):T
	{
		return t;
	}

	@:overload public function someField(c:Class<T>):Int
	{
		return 51;
	}
}

class ChildJava3<A, T : BaseJava> extends ChildJava2<T>
{
	public var initialized3 = true;

	@:overload override public function someField(t:T):T
	{
		return null;
	}

	@:overload public function someField<Z>(a:A, t:T, z:Z):Z
	{
		return z;
	}

	@:overload public function someField(a:A, c:Int):Int
	{
		return 52;
	}
}

class ChildJava4<X, Y, Z : ChildJava2<Dynamic>> extends ChildJava3<Y, Z>
{
}

interface NormalInterface
{
	function someField(i:Bool):Int;
}

interface OverloadedInterface extends NormalInterface
{
	@:overload function someField(s:String):Int;
	@:overload function someField(f:Float):Int;
}

