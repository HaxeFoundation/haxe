package unit;
import haxe.io.Bytes;
import haxe.test.Base;
import haxe.test.Base.Base_InnerClass;
import haxe.test.TEnum;

//C#-specific tests, like unsafe code
class TestCSharp extends Test
{
#if cs

	// -net-lib tests
	function testHaxeKeywords()
	{
		eq(Base._inline, 42);
		eq(Base._callback, 43);
		eq(Base._cast, 44);
		eq(Base._untyped, 45);
		Base._untyped = 40;
		eq(Base._untyped, 40);
	}

	function testTypes()
	{
		eq(Base.charTest(cast 10), cast 10);
		eq(Base.byteTest(cast 10), cast 10);
	}

	function testInnerClass()
	{
		//-java-lib should be able to detect inner classes on import
		var i = new Base_InnerClass();
		eq(10,i.nameClash());

		var i2 = new Base_InnerClass_InnerInnerClass();
		t(true);
	}

	function testGenerics()
	{
		// t(haxe.test.GenericHelper.staticTypedGeneric(new Base_InnerClass_InnerInnerClass()) != null);

		var helper = new haxe.test.GenericHelper();

		var val = new Base_InnerClass();
		var g1 = new haxe.test.Generic1_1(val);
		g1.complexTypeParameterOfTypeParameter(new Base_InnerClass_InnerInnerClass());
		//if no compile-time error, we're fine!
		t(true);
	}

	function testDelegates()
	{
		var run = false;
		var v:haxe.test.VoidVoid = function () run = true;
		f(run);
		v.Invoke();
		t(run);
		f(didRun);
		v = doRun;
		v.Invoke();
		t(didRun);

		run = false;
		var someFunc = function() run = true;
		f(run);
		v = someFunc;
		f(run);
		v.Invoke();
		t(run);

		var someFunc2 = someFunc;
		var getFun = function() return someFunc2;
		run = false;
		f(run);
		v = { var x = "complex body"; getFun(); };
		f(run);
		v.Invoke();
		t(run);

		//var dyn:Dynamic = v;
		//t(Std.is(dyn, haxe.test.VoidVoid));
	}

	var didRun = false;
	function doRun()
	{
		didRun = true;
	}

	function testOverloadOverride()
	{
		var c = new haxe.test.MyClass();
		eq(42,c.SomeProp);
		eq(42,c.SomeProp2);

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
		eq(21,c.SomeProp);
		t(c.getCalled);
		eq(21,c.SomeProp2);

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
		eq(21,c.SomeProp);
		t(c.getCalled);
		eq(21,c.SomeProp2);
	}

	function testEnum()
	{
		var e = TEnum.TA;
		switch(e)
		{
			case TA:
				t(true);
			case _:
				t(false);
		}
		eq("TA",Type.enumConstructor(e));
	}

	@:skipReflection private function refTest(i:cs.Ref<Int>):Void
	{
		i *= 2;
	}

	@:skipReflection private function outTest(out:cs.Out<Int>, x:Int):Void
	{
		out = x * 2;
	}

	// test for https://github.com/HaxeFoundation/haxe/issues/2528
	public function testDynObjectSetField()
	{
		var a:Dynamic = {};
		a.status = 10;
		var b:{status:Int} = a;
		b.status = 15;

		eq(a, b);
		eq(Reflect.fields(a).length, 1);
		eq(Reflect.fields(b).length, 1);
		eq(a.status, 15);
		eq(b.status, 15);
	}

	public function testRef()
	{
		var i = 10;
		refTest(i);
		eq(i, 20);

		var cl:NativeClass = new HxClass();
		cl.refTest(i);
		eq(i, 80);

		cl.test = 100;
		cl.refTest(cl.test);
		eq(cl.test,400);
	}

	public function testOut()
	{
		var i = 0;
		outTest(i, 10);
		eq(i, 20);

		var cl:NativeClass = new HxClass();
		cl.outTest(i, 10);
		eq(i, 40);

		cl.test = 20;
		cl.outTest(cl.test, 10);
		eq(cl.test,40);
	}

	public function testChecked()
	{
		exc(function()
				{
					cs.Lib.checked({
						var x = 1000;
						while(true)
					{
						x *= x;
					}
					});
				});
	}

	public function testUncheckedAttribute()
	{
		var cls = cs.Lib.toNativeType( TestMyClass ),
				attribType = cs.Lib.toNativeType( cs.system.componentmodel.DescriptionAttribute );
		var attrib:cs.system.componentmodel.DescriptionAttribute = cast cs.system.Attribute.GetCustomAttribute(cls,attribType,true);
		t(attrib != null);
		eq("MyClass Description", attrib.Description);

		attrib = cast cs.system.Attribute.GetCustomAttribute(cls.GetMethod("argumentDescription"), attribType,true);
		t(attrib != null);
		eq("Argument description", attrib.Description);

		attrib = cast cs.system.Attribute.GetCustomAttribute(cls.GetMethod("argumentDescription").GetParameters()[0], attribType,true);
		t(attrib != null);
		eq("Type description test", attrib.Description);
	}

	public function testEvents()
	{
		var x = new haxe.test.MyClass();
		var hasFired = false;
		f(hasFired);
		var fn:haxe.test.VoidVoid = function() hasFired = true;
		x.add_voidvoid( fn );
		f(hasFired);
		x.dispatch();
		t(hasFired);
		hasFired = false;
		x.dispatch();
		t(hasFired);
		hasFired = false;
		x.remove_voidvoid( fn );
		x.dispatch();
		f(hasFired);

		var hasFired = false;
		f(hasFired);
		var fn:haxe.test.VoidVoid = function() hasFired = true;
		haxe.test.MyClass.add_voidvoid2( fn );
		f(hasFired);
		haxe.test.MyClass.dispatch2();
		t(hasFired);
		hasFired = false;
		haxe.test.MyClass.dispatch2();
		t(hasFired);
		hasFired = false;
		haxe.test.MyClass.remove_voidvoid2( fn );
		haxe.test.MyClass.dispatch2();
		f(hasFired);
	}

#if unsafe

	@:unsafe public function testUnsafe()
	{
		var x:cs.NativeArray<Int> = new cs.NativeArray(10);
		cs.Lib.fixed({
			var p = cs.Lib.pointerOfArray(x);
			for (i in 0...10)
		{
			p[0] = i;
			p = p.add(1);
		}
		});

		cs.Lib.fixed( {
			var p = cs.Lib.pointerOfArray(x);
			for (i in 0...10)
		{
			eq(p[i], i);
		}
		});

		var x:Int = 0;
		var addr = cs.Lib.addressOf(x);
		eq(cs.Lib.valueOf(addr), 0);
		eq(addr[0], 0);
		addr[0] = 42;
		eq(cs.Lib.valueOf(addr), 42);
		eq(addr[0], 42);
		eq(x, 42);


	}

#end

	// test these because C# generator got a special filter for these expressions
	public function testNullConstEq()
	{
		var a:Null<Int> = 10;
		f(a == null);
		f(null == a);
		t(a != null);
		t(null != a);
	}

#end
}

@:nativeGen private class NativeClass
{
	public var test:Int;
	public function outTest(out:cs.Out<Int>, x:Int):Void
	{
		out = x * 2;
	}

	public function refTest(i:cs.Ref<Int>):Void
	{
		i *= 2;
	}
}

@:meta(System.ComponentModel.Description("Type description test"))
typedef StringWithDescription = String;

private class HxClass extends NativeClass
{

	public function new()
	{

	}

	//here it would normally fail due to the added fast reflection field
	override public function outTest(out:cs.Out<Int>, x:Int):Void
	{
		out = x * 4;
	}

	override public function refTest(i:cs.Ref<Int>):Void
	{
		super.refTest(i);
		i *= 2;
	}
}

@:meta(System.ComponentModel.Description("MyClass Description"))
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
	public var getCalled:Bool;

	@:meta(System.ComponentModel.Description("Argument description"))
	@:keep public function argumentDescription(arg:StringWithDescription)
	{
	}

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

	@:overload override private function get_SomeProp():Int
	{
		getCalled = true;
		return 21;
	}

	@:overload override private function get_SomeProp2():Int
	{
		return Std.int(super.get_SomeProp2() / 2);
	}
}
