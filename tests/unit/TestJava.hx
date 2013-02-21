package unit;
import haxe.io.Bytes;
import haxe.test.Base;
import haxe.test.Base.Base_InnerClass;

#if java
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

interface NormalInterface
{
	function someField(i:Bool):Int;
}

interface OverloadedInterface extends NormalInterface
{
	@:overload function someField(s:String):Int;
	@:overload function someField(f:Float):Int;
}

class TestJava extends Test
{
  function textException()
  {
    var native = new NativeClass();
    var hx:NativeClass = new HxClass();

    exc(function() try native.excTest() catch (e:Dynamic) throw e);
    var dyn:Dynamic = native;
    exc(dyn.excTest);

    try
      hx.excTest()
    catch(e:Dynamic) throw e; //shouldn't throw any exception
  }

	function testOverload()
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

		var child:ChildJava3<Bool, BaseJava> = new ChildJava3(Bytes.ofString("test"));
		eq(child.s, "test");
		eq(child.someField(base), null);
		eq(child.someField(true, child, 99), 99);
		eq(child.someField(true, 10), 52);
	}

	function testHaxeKeywords()
	{
		eq(Base._inline, 42);
		eq(Base._callback, 43);
		eq(Base._cast, 44);
		eq(Base._untyped, 45);
		eq(Base._in, 46);
		Base._in = 40;
		eq(Base._in, 40);
	}


	function testInnerClass()
	{
		//-java-lib should be able to detect inner classes on import
		var i = new Base_InnerClass();
		eq(i.nameClash(), 10);

		var i2 = new Base_InnerClass_InnerInnerClass();
		t(true);
	}

	function testGenerics()
	{
		t(haxe.test.GenericHelper.staticTypedGeneric(Base_InnerClass_InnerInnerClass) != null);

		var helper = new haxe.test.GenericHelper();
		//TODO use typedAs
		eq(helper.getUntypedGeneric(), null);
		eq(helper.typedGeneric, null);

		var val = new Base_InnerClass();
		var g1 = new haxe.test.Generic1(val);
		g1.complexTypeParameterOfTypeParameter(new Base_InnerClass_InnerInnerClass());
		//if no compile-time error, we're fine!
		t(true);
	}

	function testNameClash()
	{
		eq(Base._nameClash(null), -1);
		eq(new Base().nameClash(), 1);
		eq(new Base().varNameClash(1), 1);
		eq(Base._varNameClash(10.4), 10.4);

	}

	function testOverloadOverride()
	{
		var c = new TestMyClass();
		c.normalOverload(true);
		t(c.boolCalled);
		c.normalOverload(10);
		t(c.intCalled);
		c.normalOverload(haxe.Int64.ofInt(0));
		t(c.int64Called);
		c.normalOverload("");
		t(c.stringCalled);
		c.normalOverload({});
		t(c.dynamicCalled);

		var c = new TestMyClass("");
		t(c.alternativeCtorCalled);
		var b:haxe.test.MyClass = c;
		b.normalOverload(true);
		t(c.boolCalled);
		b.normalOverload(10);
		t(c.intCalled);
		b.normalOverload(haxe.Int64.ofInt(0));
		t(c.int64Called);
		b.normalOverload("");
		t(c.stringCalled);
		b.normalOverload({});
		t(c.dynamicCalled);
	}

	function testMiscJavaLib()
	{
		//setting inline should be an error
		t(TestType.typeError(Base.inlineNumber = 4));
	}

	//TODO:
	//overload with functions + variable types

}

private class TestMyClass extends haxe.test.MyClass
{
	@:overload public function new()
	{
		super();
	}

	@:overload public function new(str:String)
	{
		super();
		alternativeCtorCalled = true;
	}

	public var alternativeCtorCalled:Bool;
	public var boolCalled:Bool;
	public var intCalled:Bool;
	public var int64Called:Bool;
	public var stringCalled:Bool;
	public var dynamicCalled:Bool;

	@:overload override public function normalOverload(b:Bool):Void
	{
		this.boolCalled = true;
	}

	@:overload override public function normalOverload(i:Int):Void
	{
		this.intCalled = true;
	}

	@:overload override public function normalOverload(i64:haxe.Int64):Void
	{
		this.int64Called = true;
	}

	@:overload override public function normalOverload(str:String):Void
	{
		this.stringCalled = true;
	}

	@:overload override public function normalOverload(dyn:Dynamic):Void
	{
		this.dynamicCalled = true;
	}
}

@:nativeGen private class NativeClass
{
  public function new()
  {

  }

  @:throws("java.lang.Throwable")
  public function excTest():Void
  {
    throw new java.lang.Throwable("test", null);
  }
}

private class HxClass extends NativeClass
{

  @:throws("java.lang.Throwable")
  override public function excTest():Void
  {

  }
}

#end
