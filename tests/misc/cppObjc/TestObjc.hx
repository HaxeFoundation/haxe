import utest.Assert;
import utest.ui.Report;
import utest.ui.common.HeaderDisplayMode;
class TestObjc extends utest.Test
{
	static function main()
	{
		var x:TestObjc = null;
		var c:TestClass = null;
		var runner = new utest.Runner();
		runner.addCase(new TestObjc());
		var report = Report.create(runner);
		report.displayHeader = AlwaysShowHeader;
		report.displaySuccessResults = NeverShowSuccessResults;
		runner.run();
	}

	var cls:TestClass;

	public function testCall()
	{
		Assert.equals(TestClass.aStatic(), 42);
		cls = TestClass.alloc().init();
		Assert.equals(cls.getOtherThing(), 0);
		cls.setOtherThing(42);
		// Assert.equals(cls.otherThing, 42);
		Assert.equals(cls.getOtherThing(), 42);
		Assert.equals(cls.getOtherThingChar(), 42);
		Assert.equals(cls.isBiggerThan10(2), false);
		Assert.equals(cls.isBiggerThan10(12), true);
		Assert.equals(cls.isBiggerThan10Int(3), false);
		Assert.equals(cls.isBiggerThan10Int(14), true);
		Assert.equals(cls.isBiggerThan10Num(3).boolValue(), false);
		Assert.equals(cls.isBiggerThan10Num(14).boolValue(), true);
		Assert.equals(cls.addHello("World"), "Hello, World");
		cls.something = " test";
		Assert.equals(cls.something, " test");
		Assert.equals(cls.addSomething("Hey,"), "Hey, test");
		Assert.equals(cls.addHelloAndString("World"," it works"), "Hello, World it works");
		cls.release();
	}

	public function testVar()
	{
		cls = TestClass.alloc().init();
		cls.setOtherThing(142);
		// Assert.equals(cls.otherThing, 142);
		Assert.equals(cls.getOtherThing(), 142);
		cls.release();
	}

	public function testBoxing()
	{
		cls = TestClass.alloc().init();

		cls.setOtherThing(255);

		var dyn:Dynamic = cls;
		Assert.isTrue(dyn != null);
		Assert.isTrue(cls != null);

		var someObjDecl = { a:10, b: cls };
		dyn = someObjDecl; // don't let Haxe inline that TObjectDecl
		Assert.equals(someObjDecl.b.getOtherThing(), 255);
		Assert.equals(getFieldB(someObjDecl).getOtherThing(), 255);
		cls = someObjDecl.b;
		Assert.isTrue(someObjDecl.b == cls);
		dyn = cls;

		cls.release();
		cls = null;
		Assert.isTrue(cls == null);
		cls = dyn;

		Assert.equals(cls.getOtherThing(), 255);
		cls = null;
		dyn = null;
		dyn = cls;
		Assert.isTrue(dyn == null);
		Assert.equals(dyn,null);
		Assert.isTrue(dyn == cls);
		Assert.equals(dyn,cls);
		cls.release();
	}

	static function getFieldB<T>(d:{ a:Int, b: T }):T
		return d.b;

	public function testNull()
	{
		Assert.isTrue(TestClass.isNull(null));
		Assert.isFalse(TestClass.isNull(TestClass.alloc().init()));
	}

	public function testInterface()
	{
		cls = TestClass.alloc().init();
		cls.setOtherThing(21);
		Assert.isTrue(cls.getSelf() == cls);
		Assert.equals(cls.getSelf(), cls);

		var iface:TestInterface = cls;
		var obj:Dynamic = iface;
		Assert.isTrue(iface == cls);
		Assert.equals(iface, cls);
		Assert.isTrue(obj == cls);
		Assert.equals(obj, cls);
		Assert.equals(iface.getSelf(), cls);
		Assert.equals(iface.getSelf(), cls.getSelf());

		Assert.equals(iface.getOtherThing(), 21);
		Assert.equals(iface.getOtherThingChar(), 21);
		cls.setOtherThing(100);
		Assert.equals(iface.getOtherThing(), 100);
		Assert.equals(iface.getOtherThingChar(), 100);

		Assert.equals("someOptionalMethod!",iface.someOptionalMethod());

		cls.release();
	}
}

@:include("./native/include/test.h")
@:objc extern interface TestInterface
{
	function getSelf():TestInterface;
	function getOtherThing():Int;
	function getOtherThingChar():cpp.Int8;

	@:optional function someOptionalMethod():NSString;
	@:optional function unimplementedOptional():NSString;
}

@:include("./native/include/test.h")
@:sourceFile("./native/test.m")
@:objc extern class TestClass implements TestInterface
{
	static function aStatic():Int;
	static function isNull(t:TestClass):Bool;

	static function alloc():TestClass;
	function init():TestClass;

	var something(get,set):NSString;
	var otherThing:Int;

	@:native("something") private function get_something():NSString;
	@:native("setSomething") private function set_something(value:NSString):NSString;

	function setOtherThing(value:Int):Void;
	function getOtherThing():Int;
	function getOtherThingChar():cpp.Int8;
	function addHello(str:NSString):NSString;
	@:native("addHello:andString") function addHelloAndString(str:NSString, str2:NSString):NSString;
	function addSomething(str:NSString):NSString;
	function isBiggerThan10(value:NSNumber):Bool;
	function isBiggerThan10Num(value:NSNumber):NSNumber;
	function isBiggerThan10Int(integer:Int):Bool;

	function release():Void;
	function retainCount():Int;

	function getSelf():TestClass;

	function someOptionalMethod():NSString;

	@:deprecated('This method is not implemented on this class')
	@:noCompletion @:optional function unimplementedOptional():NSString;

	@:plain static function some_c_call(t:TestClass):Int;
	@:plain static function is_bigger_than_10(t:TestClass, val:Int):Bool;
}

@:forward abstract NSString(_NSString) from _NSString to _NSString
{
	@:from @:extern inline public static function fromString(str:String):NSString
		return _NSString.stringWithUTF8String(str);

	@:to @:extern inline public function toString():String
		return this.UTF8String();
}

@:native("NSString") @:objc extern class _NSString
{
	static function stringWithUTF8String(str:cpp.CastCharStar):NSString;

	function UTF8String():cpp.ConstCharStar;
}

@:forward abstract NSNumber(_NSNumber) from _NSNumber to _NSNumber
{
	@:from @:extern inline public static function fromInt(i:Int):NSNumber
		return _NSNumber.numberWithInt(i);

	@:to @:extern inline public function toInt():Int
		return this.intValue();

	@:to @:extern inline public function toBool():Bool
		return this.boolValue();
}

@:native("NSNumber") @:objc extern class _NSNumber
{
	static function numberWithInt(i:Int):NSNumber;
	function intValue():Int;
	function boolValue():Bool;
}
