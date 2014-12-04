package unit;
import haxe.io.Bytes;
import haxe.test.Base;
import haxe.test.Base.Base_InnerClass;
import haxe.test.TEnum;
import haxe.test.TEnumWithValue;
import NoPackage;
#if unsafe
import cs.Pointer;
#end

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

#if unsafe
	@:unsafe function testBoxedPointer()
	{
		var ptr:Pointer<Int> = cast cs.system.runtime.interopservices.Marshal.AllocHGlobal(10 * 4).ToPointer();
		ptr[0] = 0;
		eq(ptr[0],0);
		ptr[0] = -1;
		eq(ptr[0],-1);

		var dyn:Dynamic = ptr;
		ptr = null;
		ptr = dyn;
		eq(ptr[0],-1);

		var arr = [ptr];
		eq(arr[0][0], -1);

		var captured = ptr;
		function test(v:Int) captured[0] = v;

		test(0xFFFF);
		eq(ptr[0],0xFFFF);
		eq(captured[0],0xFFFF);
		t(ptr == captured);

		var other:Pointer<Pointer<Int>> = cast cs.system.runtime.interopservices.Marshal.AllocHGlobal(10 * 4).ToPointer();
		other[0] = ptr;
		eq(other[0][0], 0xFFFF);
		var captured = other;
		function test(v:Int) captured[0][0] = v;
		test(-1);
		eq(other[0][0],-1);
		eq(captured[0][0],-1);
		t(other == captured);

		function test2(p:Pointer<Pointer<Int>>, v:Int)
			p[0][0]=v;

		test2(other,-2);
		eq(other[0][0],-2);
	}

	@:unsafe function testPointerAccess()
	{
		var struct = new SomeStruct(10,20);
		eq(10,struct.int);
		eq(20.0,struct.float);

		var addr = cs.Lib.addressOf(struct);
		eq(10,addr.acc.int);
		eq(20.0,addr.acc.float);
		addr.acc.int = 22;
		eq(22,addr.acc.int);
		eq(22,struct.int);

		addr.acc.float = 42.42;
		eq(42.42,addr.acc.float);
		eq(42.42,struct.float);

		var arr = new cs.NativeArray<SomeStruct>(10);
		cs.Lib.fixed({
			var arrptr = cs.Lib.pointerOfArray(arr);
			for (i in 0...10)
			{
				(arrptr + i).acc.int = i;
				(arrptr + i).acc.float = i + i / 10;
			}
			var ptr = arrptr;
			for (i in 0...10)
			{
				eq(arr[i].int,i);
				eq(arr[i].float,i + i / 10);

				eq((arrptr + i).acc.int, i);
				eq((arrptr + i).acc.float, i + i / 10);

				ptr.acc.int = i *2;
				ptr.acc.float = (i + i / 10) * 2;
				ptr++;
			}
			for (i in 0...10)
			{
				eq(arr[i].int,i*2);
				eq(arr[i].float,(i + i / 10)*2);

				eq((arrptr + i).acc.int, i*2);
				eq((arrptr + i).acc.float, (i + i / 10)*2);
			}
		});
	}
#end

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

		var noPack:NoPackage = new NoPackage();
		t(noPack.isWorking);
		t(noPack.b != null);
		t(noPack.b.isReallyWorking);

		var noPack2 = new NoPackage.NoPackage_NoPackInner();
		t(noPack2.isReallyWorking);
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

		eq(0, Type.enumIndex(TEnum.TA));
		eq(0, Type.enumIndex(getTA()));
		eq(3, Type.enumIndex(TEnumWithValue.TVA));
		eq(3, Type.enumIndex(getTVA()));

		eq(0, Type.enumIndex(TEnumWithValue.TVB));
		eq(0, Type.enumIndex(getTVB()));
		eq(1, Type.enumIndex(TEnum.TB));
		eq(1, Type.enumIndex(getTB()));

		eq(2, Type.enumIndex(TEnum.TC));
		eq(2, Type.enumIndex(getTC()));
		eq(2, Type.enumIndex(TEnumWithValue.TVC));
		eq(2, Type.enumIndex(getTVC()));

		eq(1, Type.enumIndex(TEnumWithValue.TVD));
		eq(1, Type.enumIndex(getTVD()));

		checkEnum(TEnum,0,TEnum.TA);
		checkEnum(TEnum,1,TEnum.TB);
		checkEnum(TEnum,2,TEnum.TC);

		checkEnum(TEnumWithValue,3,TEnumWithValue.TVA);
		checkEnum(TEnumWithValue,0,TEnumWithValue.TVB);
		checkEnum(TEnumWithValue,2,TEnumWithValue.TVC);
		checkEnum(TEnumWithValue,1,TEnumWithValue.TVD);
	}

	private static function getArray(arr:cs.system.Array)
	{
		return [ for (i in 0...arr.Length) arr.GetValue(i) ];
	}

	function checkEnum<T>(e:Enum<T>,idx:Int,v:T,?pos:haxe.PosInfos)
	{
		eq(v,Type.createEnumIndex(e,idx),pos);
	}

	function getTA() return TEnum.TA;
	function getTVA() return TEnumWithValue.TVA;
	function getTB() return TEnum.TB;
	function getTVB() return TEnumWithValue.TVB;
	function getTC() return TEnum.TC;
	function getTVC() return TEnumWithValue.TVC;
	function getTVD() return TEnumWithValue.TVD;

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

		i = 10;
		var cl = new haxe.test.MyClass();
		cl.refTest(i);
		eq(i,420);
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

		var cl = new haxe.test.MyClass();
		cl.outTest(i);
		eq(i,42);
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

@:struct @:nativeGen private class SomeStruct
{
	public var int:Int;
	public var float:Float;

	public function new(i,f)
	{
		this.int = i;
		this.float = f;
	}
}
