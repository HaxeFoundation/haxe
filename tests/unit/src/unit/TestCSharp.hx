package unit;
import haxe.io.Bytes;
import haxe.test.Base;
import haxe.test.Base.Base_InnerClass;
import haxe.test.TEnum;
import haxe.test.TEnumWithValue;
import haxe.test.TEnumWithBigValue;
import haxe.test.TEnumWithFlag;
import haxe.test.TEnumWithBigFlag;
import haxe.test.IEditableTextBuffer;
import haxe.test.LowerCaseClass;

import cs.Flags;
import cs.system.componentmodel.DescriptionAttribute;

import NoPackage;
#if unsafe
import cs.Pointer;
#end
import cs.system.Action_1;

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

	function testIssue3474()
	{
		var a:IEditableTextBuffer = cast null;
		eq(a,null);
		var didRun = false;
		try
		{
			eq(a.Property, "should not succeed");
		}
		catch(e:Dynamic)
		{
			didRun = true;
		}

		t(didRun);
	}

	function testLowerCase()
	{
		var l = new LowerCaseClass();
		t(l.works);
	}

	function testGetItem()
	{
		var b = new Base();
		eq(b[1], 20);
		eq(b.get_Item(2,3), 6);
		var dyn:Dynamic = b;
		eq(dyn[1], 20);
		eq(dyn.get_Item(2,3), 6);

		var b:Base = new Base_InnerClass();
		eq(b[1], 20);
		eq(b.get_Item(2,3), 6);
		var dyn:Dynamic = b;
		eq(dyn[1], 20);
		eq(dyn.get_Item(2,3), 6);
	}

	// function testOptional()
	// {
	// 	eq(new Base().optional(), 420);
	// 	eq(new Base().optional(10), 100);
	// }

	function testProp()
	{
		var b = new Base();
		eq(b.prop, "SomeValue");
		var dyn:Dynamic = b;
		eq(dyn.prop, "SomeValue");
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
		//t((dyn is haxe.test.VoidVoid));
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

	function testEnumFlags()
	{
		var flags = new Flags(TFA) | TFC;
		t(flags.has(TFA));
		t(flags.has(TFC));
		f(flags.has(TFB));
		f(flags.has(TFD));
		flags = new Flags();
		f(flags.has(TFA));
		f(flags.has(TFB));
		f(flags.has(TFC));
		f(flags.has(TFD));

		flags |= TFB;
		t(flags.has(TFB));
		eq(flags & TFB,flags);
		flags |= TFA;

		f(flags.has(TFD));
		t(flags.hasAny(new Flags(TFD) | TFB));
		f(flags.hasAny(new Flags(TFD) | TFC));
		f(flags.hasAll(new Flags(TFD) | TFB));
		t(flags.hasAll(new Flags(TFB)));
		t(flags.hasAll(new Flags(TFA) | TFB));

		var flags = new Flags(TFBA) | TFBC;
		t(flags.has(TFBA));
		t(flags.has(TFBC));
		f(flags.has(TFBB));
		f(flags.has(TFBD));
		flags = new Flags();
		f(flags.has(TFBA));
		f(flags.has(TFBB));
		f(flags.has(TFBC));
		f(flags.has(TFBD));

		flags |= TFBB;
		t(flags.has(TFBB));
		eq(flags & TFBB,flags);
		flags |= TFBA;

		f(flags.has(TFBD));
		t(flags.hasAny(new Flags(TFBD) | TFBB));
		f(flags.hasAny(new Flags(TFBD) | TFBC));
		f(flags.hasAll(new Flags(TFBD) | TFBB));
		t(flags.hasAll(new Flags(TFBB)));
		t(flags.hasAll(new Flags(TFBA) | TFBB));
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

		eq(Type.enumIndex(getTA()), Type.enumIndex(TEnum.TA));
		eq(Type.enumIndex(getTVA()), Type.enumIndex(TEnumWithValue.TVA));
		eq(Type.enumIndex(getTBA()), Type.enumIndex(TEnumWithBigValue.TBA));

		eq(Type.enumIndex(getTVB()), Type.enumIndex(TEnumWithValue.TVB));
		eq(Type.enumIndex(getTBB()), Type.enumIndex(TEnumWithBigValue.TBB));
		eq(Type.enumIndex(getTB()), Type.enumIndex(TEnum.TB));

		eq(Type.enumIndex(getTC()), Type.enumIndex(TEnum.TC));
		eq(Type.enumIndex(getTVC()), Type.enumIndex(TEnumWithValue.TVC));
		eq(Type.enumIndex(getTBC()), Type.enumIndex(TEnumWithBigValue.TBC));

		eq(Type.enumIndex(getTVD()), Type.enumIndex(TEnumWithValue.TVD));
		eq(Type.enumIndex(getTBD()), Type.enumIndex(TEnumWithBigValue.TBD));

		checkEnum(TEnum,TEnum.TA);
		checkEnum(TEnum,TEnum.TB);
		checkEnum(TEnum,TEnum.TC);

		checkEnum(TEnumWithValue,TEnumWithValue.TVA);
		checkEnum(TEnumWithValue,TEnumWithValue.TVB);
		checkEnum(TEnumWithValue,TEnumWithValue.TVC);
		checkEnum(TEnumWithValue,TEnumWithValue.TVD);

		checkEnum(TEnumWithBigValue,TEnumWithBigValue.TBA);
		checkEnum(TEnumWithBigValue,TEnumWithBigValue.TBB);
		checkEnum(TEnumWithBigValue,TEnumWithBigValue.TBC);
		checkEnum(TEnumWithBigValue,TEnumWithBigValue.TBD);

		//issue #2308
		var fn = getEnumValue;
		eq(0x100, Reflect.callMethod(null, fn, [TEnumWithValue.TVA]));
	}

	static function getEnumValue(e:TEnumWithValue):Int
	{
		return cast e;
	}

	private static function getArray(arr:cs.system.Array)
	{
		return [ for (i in 0...arr.Length) arr.GetValue(i) ];
	}

	function checkEnum<T : EnumValue>(e:Enum<T>,v:T,?pos:haxe.PosInfos)
	{
		var idx = Type.enumIndex(v);
		eq(v,Type.createEnumIndex(e,idx),pos);
	}

	function getTA() return TEnum.TA;
	function getTVA() return TEnumWithValue.TVA;
	function getTBA() return TEnumWithBigValue.TBA;
	function getTB() return TEnum.TB;
	function getTVB() return TEnumWithValue.TVB;
	function getTBB() return TEnumWithBigValue.TBB;
	function getTC() return TEnum.TC;
	function getTVC() return TEnumWithValue.TVC;
	function getTBC() return TEnumWithBigValue.TBC;
	function getTVD() return TEnumWithValue.TVD;
	function getTBD() return TEnumWithBigValue.TBD;

	@:skipReflection private function refTest(i:cs.Ref<Int>):Void
	{
		i *= 2;
	}

	@:skipReflection private function refTestAssign(i:cs.Ref<Int>):Void
	{
		i = 2;
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
		refTestAssign(i);
		eq(i, 2);

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

	function testHaxeEvents() {
		var c = new EventClass();
		var sum = 0;
		var cb:Action_1<Int> = function(x) sum += x;
		c.add_Event1(cb);
		c.invokeEvent1(1);
		c.invokeEvent1(2);
		c.remove_Event1(cb);
		c.invokeEvent1(3);
		eq(sum, 3);

		c.add_Event2(cb);
		eq(c.event2Counter, 1);
		c.remove_Event2(cb);
		eq(c.event2Counter, 0);

		sum = 0;
		EventClass.add_SEvent1(cb);
		EventClass.invokeSEvent1(1);
		EventClass.invokeSEvent1(2);
		EventClass.remove_SEvent1(cb);
		EventClass.invokeSEvent1(3);
		eq(sum, 3);

		EventClass.add_SEvent2(cb);
		eq(EventClass.sEvent2Counter, 1);
		EventClass.remove_SEvent2(cb);
		eq(EventClass.sEvent2Counter, 0);

		var i:IEventIface = c;
		sum = 0;
		i.add_IfaceEvent1(cb);
		c.invokeIfaceEvent1(1);
		c.invokeIfaceEvent1(2);
		i.remove_IfaceEvent1(cb);
		c.invokeIfaceEvent1(3);
		eq(sum, 3);

		i.add_IfaceEvent2(cb);
		eq(c.ifaceEvent2Counter, 1);
		i.remove_IfaceEvent2(cb);
		eq(c.ifaceEvent2Counter, 0);
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

@:strict(DescriptionAttribute("Type description test"))
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

@:strict(cs.system.componentmodel.DescriptionAttribute("MyClass Description"))
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

	@:strict(DescriptionAttribute("Argument description"))
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

private interface IEventIface {
	@:keep
    @:event private var IfaceEvent1:Action_1<Int>;
    function add_IfaceEvent1(cb:Action_1<Int>):Void;
    function remove_IfaceEvent1(cb:Action_1<Int>):Void;

    @:keep
    @:event private var IfaceEvent2:Action_1<Int>;
    function add_IfaceEvent2(cb:Action_1<Int>):Void;
    function remove_IfaceEvent2(cb:Action_1<Int>):Void;
}

@:publicFields
private class EventClass implements IEventIface {
	function new() {}

    @:event private var Event1:Action_1<Int>;
    function add_Event1(cb:Action_1<Int>) {}
    function remove_Event1(cb:Action_1<Int>) {}
    function invokeEvent1(i) if (Event1 != null) Event1.Invoke(i);

    @:event private var Event2:Action_1<Int>;
    var event2Counter = 0;
    function add_Event2(cb:Action_1<Int>) event2Counter++;
    function remove_Event2(cb:Action_1<Int>) event2Counter--;

    @:event private static var SEvent1:Action_1<Int>;
    static function add_SEvent1(cb:Action_1<Int>) {}
    static function remove_SEvent1(cb:Action_1<Int>) {}
    static function invokeSEvent1(i) if (SEvent1 != null) SEvent1.Invoke(i);

    @:event private static var SEvent2:Action_1<Int>;
    static var sEvent2Counter = 0;
    static function add_SEvent2(cb:Action_1<Int>) sEvent2Counter++;
    static function remove_SEvent2(cb:Action_1<Int>) sEvent2Counter--;

    @:event private var IfaceEvent1:Action_1<Int>;
    function add_IfaceEvent1(cb:Action_1<Int>) {}
    function remove_IfaceEvent1(cb:Action_1<Int>) {}
    function invokeIfaceEvent1(i) if (IfaceEvent1 != null) IfaceEvent1.Invoke(i);

    @:event private var IfaceEvent2:Action_1<Int>;
    var ifaceEvent2Counter = 0;
    function add_IfaceEvent2(cb:Action_1<Int>) ifaceEvent2Counter++;
    function remove_IfaceEvent2(cb:Action_1<Int>) ifaceEvent2Counter--;
}
