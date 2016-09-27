package unit;
import haxe.io.Bytes;
import haxe.test.Base;
import haxe.test.MyClass;
import haxe.test.Base.Base_InnerClass;
import haxe.test.Base.Base___InnerClass3__;
import haxe.test.Base.Base___InnerClass3___InnerClass4__;
import haxe.test.TEnum;
import haxe.test.LowerCaseClass;
import java.util.EnumSet;
import java.vm.*;

#if java
@:strict(haxe.test.MyClass.MyClass_MyAnnotation({ author:"John Doe", someEnum: TB }))
@:strict(MyClass_ParameterLessAnnotation)
class TestJava extends Test
{
  function testException()
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

	public function testIssue2964()
	{
		var cl = new MyClass();
		var bbool:java.lang.Boolean = null;
		eq(cl.boolTest1(bbool), 100);
		eq(cl.boolTest1(true), true);
		eq(cl.boolTest1(false), false);
		bbool = true;
		eq(cl.boolTest1(bbool), 1);
		bbool = false;
		eq(cl.boolTest1(bbool), 0);
		eq(cl.boolTest2(null), 100);
		eq(cl.boolTest2(true), 1);
		eq(cl.boolTest2(false), 0);

		var i:java.lang.Integer = null;
		eq(cl.intTest1(i), 100);
		eq(cl.intTest1(cast(-1, java.lang.Integer)),-1);
		eq(cl.intTest1(cast(1000, java.lang.Integer)),1000);
		i = -1;
		eq(cl.intTest1(i), -1);
		i = null;
		eq(cl.intTest2(i), 100);
		eq(cl.intTest2(-1),-1);
		eq(cl.intTest2(1000),1000);
		i = -1;
		eq(cl.intTest2(i), -1);

		var i:java.lang.Long = null;
		eq(cl.longTest(i), 100);
		eq(cl.longTest(haxe.Int64.ofInt(-1)),-1);
		eq(cl.longTest(haxe.Int64.ofInt(1000)),1000);
		i = 10;
		eq(cl.longTest(i), 10);
	}

	public function testVarClash()
	{
		var ic = new Base_InnerClass2();
		eq(ic.varNameClash2(), 1);
		eq(ic.varNameClash2(2),2.2);
		var iface:Base_VarNameClash = ic;
		eq(iface.varNameClash2(), 1);
		eq(iface.varNameClash2(2),2.2);
		var base:Base = ic;
		eq(base.varNameClash2,0);
		base.varNameClash2 = 2;
		eq(base.varNameClash2,2);
	}

	@:strict(MyClass_MyAnnotation({ author:"author", currentRevision: 2 }))
	public function testAnnotations()
	{
		var cl = java.Lib.toNativeType(TestJava);
		var a = cl.getAnnotation(java.Lib.toNativeType(MyClass_MyAnnotation));
		t(a != null);
		eq(a.author(), "John Doe");
		eq(a.someEnum(), TB);
		eq(a.currentRevision(), 1);
		t(cl.getAnnotation(java.Lib.toNativeType(MyClass_ParameterLessAnnotation)) != null);
		var m = cl.getMethod("testAnnotations", new java.NativeArray(0));
		a = m.getAnnotation(java.Lib.toNativeType(MyClass_MyAnnotation));
		t(a != null);
		eq(a.author(), "author");
		eq(a.someEnum(), TC);
		eq(a.currentRevision(), 2);
	}

	function testLowerCase()
	{
		var l = new LowerCaseClass();
		t(l.works);
	}

	function testNameCase()
	{
		t(haxe.uppercasepackage.SomeClass.SomeClassFound);
		t(haxe.uppercasepackage.Lowercase.lowercaseFound);
	}

	function testEnumSet()
	{
		var es1:EnumSet<TEnum> = EnumSet.noneOf(java.Lib.toNativeEnum(TEnum));
		f(es1.contains(TA));
		es1.add(TA);
		t(es1.contains(TA));
		var es2 = EnumSet.of(HA,HB);
		t(es2.contains(HA));
		t(es2.contains(HB));
		f(es2.contains(HC));
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

	function testTypes()
	{
		eq(Base.charTest(cast 10), cast 10);
		eq(Base.byteTest(cast 10), cast 10);
	}


	function testInnerClass()
	{
		//-java-lib should be able to detect inner classes on import
		var i = new Base_InnerClass();
		eq(i.nameClash(), 10);

		var i2 = new Base_InnerClass_InnerInnerClass();
		t(true);

		eq("InnerName1$", haxe.test.MyClass.MyClass_InnerName1_.test());
		eq("$InnerName2", haxe.test.MyClass.MyClass__InnerName2.test());
		eq("Inner$Name3", haxe.test.MyClass.MyClass_Inner_Name3.test());
		eq("InnerName4$$", haxe.test.MyClass.MyClass_InnerName4__.test());
		eq("$$InnerName5", haxe.test.MyClass.MyClass___InnerName5.test());
		eq("Inner$$Name6", haxe.test.MyClass.MyClass_Inner__Name6.test());
		eq("$$Inner$$Name7$$", haxe.test.MyClass.MyClass___Inner__Name7__.test());
		eq("$$Inner$$Name8", haxe.test.MyClass.MyClass___Inner__Name7_____Inner__Name8.test());
	}

	function testGenerics()
	{
		var jcl:java.lang.Class<Base_InnerClass_InnerInnerClass> = cast Base_InnerClass_InnerInnerClass;
		t(haxe.test.GenericHelper.staticTypedGeneric(jcl) != null);

		var helper = new haxe.test.GenericHelper();
		//TODO use typedAs
		eq(helper.getUntypedGeneric(), null);
		eq(helper.typedGeneric, null);

		var val = new Base_InnerClass();
		var val3 = new Base___InnerClass3__();
		var g1 = new haxe.test.Generic1(val);
		g1.complexTypeParameterOfTypeParameter(new Base_InnerClass_InnerInnerClass());
		//if no compile-time error, we're fine!
		t(true);
	}

	function testNameClash()
	{
		eq(Base.nameClash(null), -1);
		eq(new Base().nameClash(), 1);
		eq(new Base().varNameClash(1), 1);
		eq(Base.varNameClash(10.4), 10.4);

	}

	function testOverloadOverride()
	{
		var c = new TestMyClass();
		c.normalOverload(true);
		t(c.boolCalled);
		c.normalOverload(6161);
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

	function testThrows1()
	{
		// test 1: no @:throws / no catch
		var b = new Base();
		eq(Base.throwsTest(), 5);
		eq(Base.throwsTest(42), 42);
		eq(b.throwsMemberTest(), 6);
		eq(b.throwsMemberTest(true), 10);
	}

	function testThrows2()
	{
		// test 2: catching only the IOException
		try
		{
			var b = new Base();
			eq(Base.throwsTest(), 5);
			eq(Base.throwsTest(42), 42);
			eq(b.throwsMemberTest(), 6);
			eq(b.throwsMemberTest(true), 10);
		}
		catch(e:java.io.IOException)
		{
		}
	}

	function testThrows3()
	{
		// test 3: catching all exceptions
		try
		{
			var b = new Base();
			eq(Base.throwsTest(), 5);
			eq(Base.throwsTest(42), 42);
			eq(b.throwsMemberTest(), 6);
			eq(b.throwsMemberTest(true), 10);
		}
		catch(e:java.lang.Throwable)
		{
		}
	}

	// test 4: @:throws IOException and only use IOException
	@:throws('java.io.IOException') function testThrows4()
	{
		var b = new Base();
		eq(Base.throwsTest(), 5);
		eq(b.throwsMemberTest(true), 10);
	}

	// test 5: @:throws IOException and use any
	@:throws('java.io.IOException') function testThrows5()
	{
		var b = new Base();
		eq(Base.throwsTest(), 5);
		eq(Base.throwsTest(42), 42);
		eq(b.throwsMemberTest(), 6);
		eq(b.throwsMemberTest(true), 10);
	}

	function testJavaLibEnum()
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

	function testMiscJavaLib()
	{
		//setting inline should be an error
		t(HelperMacros.typeError(Base.inlineNumber = 4));
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

enum HaxeEnum {
	HA;
	HB;
	HC;
}

#end
