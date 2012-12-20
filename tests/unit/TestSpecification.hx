package unit;

class TestSpecification extends Test 
{
	public function testStdIs() {
		// null subject
		var known:String = null;
		f(Std.is(known, String));
		
		var unknown = null;
		f(Std.is(unknown, String));
		
		f(Std.is(null, String));
		
		// null class
		f(Std.is("foo", null));
	}
	
	public function testStdString() {
		var cwts = new ClassWithToString();
		var cwtsc = new ClassWithToStringChild();
		var cwtsc2 = new ClassWithToStringChild2();
		
		eq(Std.string(cwts), "ClassWithToString.toString()");
		eq(Std.string(cwtsc), "ClassWithToString.toString()");
		eq(Std.string(cwtsc2), "ClassWithToStringChild2.toString()");
		
		eq(Std.string(SomeEnum.NoArguments), "NoArguments");
		eq(Std.string(SomeEnum.OneArgument("foo")), "OneArgument(foo)");
		
		eq(Std.string(null), "null");
	}
	
	public function testStdInt() {
		eq( Std.int(-1.7), -1 );
		eq( Std.int(-1.2), -1 );
		eq( Std.int(1.7), 1 );
		eq( Std.int(1.2), 1 );
		eq( Std.int(-0.7), 0 );
		eq( Std.int(-0.2), 0 );
		eq( Std.int(0.7), 0 );
		eq( Std.int(0.2), 0 );
	}
	
	public function testStdParseInt() {
		eq( Std.parseInt("0"), 0 );
		eq( Std.parseInt("   5"), 5 );
		eq( Std.parseInt("0001"), 1 );
		eq( Std.parseInt("0010"), 10 );
		eq( Std.parseInt("100"), 100 );
		eq( Std.parseInt("-100"), -100 );
		eq( Std.parseInt("100x123"), 100 );
		eq( Std.parseInt("12foo13"), 12 );
		eq( Std.parseInt(""), null );
		eq( Std.parseInt("abcd"), null );
		eq( Std.parseInt("a10"), null );
		eq( Std.parseInt(null), null );
		eq( Std.parseInt("0xFF"), 255 );
		eq( Std.parseInt("0x123"), 291 );
		eq( Std.parseInt("0XFF"), 255 );
		eq( Std.parseInt("0X123"), 291 );	
		eq( Std.parseInt("0X01"), 1 );
		eq( Std.parseInt("0x01"), 1 );
		unspec(function() Std.parseInt("0xFG"));		
	}
	
	public function testStdParseFloat() {
		eq( Std.parseFloat("0"), 0. );
		eq( Std.parseFloat("   5.3"), 5.3 );
		eq( Std.parseFloat("0001"), 1. );
		eq( Std.parseFloat("100.45"), 100.45 );
		eq( Std.parseFloat("-100.01"), -100.01 );
		eq( Std.parseFloat("100x123"), 100. );
		t( Math.isNaN(Std.parseFloat("")) );
		t( Math.isNaN(Std.parseFloat("abcd")) );
		t( Math.isNaN(Std.parseFloat("a10")) );
		t( Math.isNaN(Std.parseFloat(null)) );
		eq( Std.parseFloat("5.3 "), 5.3 );
		eq( Std.parseFloat("0.0"), 0. );
		eq( Std.parseFloat("5.3 1"), 5.3 );		
	}
	
	function testStdRandom() {
		var x = Std.random(2);
		t( x == 0 || x == 1);
		eq(Std.random(1), 0);
		eq(Std.random(0), 0);
		eq(Std.random(-100), 0);
	}	
}

private class EmptyClass {
	public function new() { }
}

private class ClassWithToString {
	public function new() { }
	public function toString() return "ClassWithToString.toString()"
}

private class ClassWithToStringChild extends ClassWithToString {
	
}

private class ClassWithToStringChild2 extends ClassWithToString {
	public override function toString() return "ClassWithToStringChild2.toString()"
}

private enum SomeEnum<T> {
	NoArguments;
	OneArgument(t:T);
}