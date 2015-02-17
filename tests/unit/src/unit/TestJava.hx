package unit;
import haxe.io.Bytes;
import haxe.test.Base;
import haxe.test.Base.Base_InnerClass;
import haxe.test.Base.Base___InnerClass3__;
import haxe.test.Base.Base___InnerClass3___InnerClass4__;
import haxe.test.TEnum;
import haxe.test.LowerCaseClass;
import java.util.EnumSet;
import java.vm.*;

#if java
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

enum HaxeEnum {
	HA;
	HB;
	HC;
}

#end
