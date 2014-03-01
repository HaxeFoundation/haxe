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
		t(haxe.test.GenericHelper.staticTypedGeneric(new Base_InnerClass_InnerInnerClass()) != null);

		var helper = new haxe.test.GenericHelper();

		var val = new Base_InnerClass();
		var g1 = new haxe.test.Generic1_1(val);
		g1.complexTypeParameterOfTypeParameter(new Base_InnerClass_InnerInnerClass());
		//if no compile-time error, we're fine!
		t(true);
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
	}

	public function testOut()
	{
		var i = 0;
		outTest(i, 10);
		eq(i, 20);

		var cl:NativeClass = new HxClass();
		cl.outTest(i, 10);
		eq(i, 40);
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
	public function outTest(out:cs.Out<Int>, x:Int):Void
	{
		out = x * 2;
	}

	public function refTest(i:cs.Ref<Int>):Void
	{
		i *= 2;
	}
}

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
