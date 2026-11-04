package unit.teststd;

class TestStd extends unit.Test {
	public function test() {
		// is
		var known:String = null;
		f((known is String));

		var unknown = null;
		f((unknown is String));
		f((null is String));
		//("foo" is null) == false;

		t(("" is String));
		t((false is Bool));
		t((1 is Int));
		f((1.5 is Int));
		t((1.5 is Float));
		t(([] is Array));
		f((cast unit.MyEnum.A is Array));

		// isOfType
		var known:String = null;
		f(Std.isOfType(known, String));

		var unknown = null;
		f(Std.isOfType(unknown, String));
		f(Std.isOfType(null, String));
		//Std.isOfType("foo", null) == false;

		t(Std.isOfType("", String));
		t(Std.isOfType(false, Bool));
		t(Std.isOfType(1, Int));
		f(Std.isOfType(1.5, Int));
		t(Std.isOfType(1.5, Float));
		t(Std.isOfType([], Array));
		f(Std.isOfType(cast unit.MyEnum.A, Array));

		// instance
		#if !js
		eq(Std.downcast("", String), "");
		#end
		var a = [];
		eq(Std.downcast(a, Array), a);
		var parent:unit.MyClass.MyParent = new MyClass.MyChild1();
		t(Std.downcast(parent, unit.MyClass.MyChild1) != null);
		eq(Std.downcast(null, Array), null);
		eq(Std.downcast(null, String), null);

		var parent:unit.MyClass.IMyParent = new MyClass.MyChild1();
		t(Std.downcast(parent, unit.MyClass.IMyChild) != null);

		// string
		var cwts = new ClassWithToString();
		var cwtsc = new ClassWithToStringChild();
		var cwtsc2 = new ClassWithToStringChild2();

		eq(Std.string(cwts), "ClassWithToString.toString()");
		eq(Std.string(cwtsc), "ClassWithToString.toString()");
		eq(Std.string(cwtsc2), "ClassWithToStringChild2.toString()");

		eq(Std.string(SomeEnum.NoArguments), "NoArguments");
		eq(Std.string(SomeEnum.OneArgument("foo")), "OneArgument(foo)");

		eq(Std.string(null), "null");

		// int
		eq(Std.int(-1.7), -1);
		eq(Std.int(-1.2), -1);
		eq(Std.int(1.7), 1);
		eq(Std.int(1.2), 1);
		eq(Std.int(-0.7), 0);
		eq(Std.int(-0.2), 0);
		eq(Std.int(0.7), 0);
		eq(Std.int(0.2), 0);

		// parseInt

		// general
		eq(Std.parseInt("0"), 0);
		eq(Std.parseInt("-1"), -1);
		// preceeding zeroes
		eq(Std.parseInt("0001"), 1);
		eq(Std.parseInt("0010"), 10);
		// trailing text
		eq(Std.parseInt("100x123"), 100);
		eq(Std.parseInt("12foo13"), 12);
		eq(Std.parseInt("23e2"), 23);
		eq(Std.parseInt("0x10z"), 16);
		eq(Std.parseInt("0x10x123"), 16);
		eq(Std.parseInt("0xff\n"), 255);
		// hexadecimals
		eq(Std.parseInt("0xff"), 255);
		eq(Std.parseInt("0x123"), 291);
		eq(Std.parseInt("0XFF"), 255);
		eq(Std.parseInt("0X123"), 291);
		eq(Std.parseInt("0X01"), 1);
		eq(Std.parseInt("0x01"), 1);
		// signs
		eq(Std.parseInt("123"), 123);
		eq(Std.parseInt("+123"), 123);
		eq(Std.parseInt("-123"), -123);
		eq(Std.parseInt("0xa0"), 160);
		eq(Std.parseInt("+0xa0"), 160);
		eq(Std.parseInt("-0xa0"), -160);
		// whitespace: space, horizontal tab, newline, vertical tab, form feed, and carriage return
		eq(Std.parseInt("   5"), 5);
		eq(Std.parseInt(" \t\n\x0b\x0c\r16"), 16);
		eq(Std.parseInt(" \t\n\x0b\x0c\r0xa"), 10);
		// whitespace and signs
		eq(Std.parseInt('  	16'), 16);
		eq(Std.parseInt('  	-16'), -16);
		eq(Std.parseInt('  	+16'), 16);
		eq(Std.parseInt('  	0x10'), 16);
		eq(Std.parseInt('  	-0x10'), -16);
		eq(Std.parseInt('  	+0x10'), 16);
		// binary and octal unsupported
		eq(Std.parseInt("010"), 10);
		eq(Std.parseInt("0b10"), 0);
		// null
		eq(Std.parseInt(null), null);
		// no number
		eq(Std.parseInt(""), null);
		eq(Std.parseInt("abcd"), null);
		eq(Std.parseInt("a10"), null);
		// invalid use of signs
		eq(Std.parseInt("++123"), null);
		eq(Std.parseInt("+-123"), null);
		eq(Std.parseInt("-+123"), null);
		eq(Std.parseInt("--123"), null);
		eq(Std.parseInt("+ 123"), null);
		eq(Std.parseInt("- 123"), null);
		eq(Std.parseInt("++0x123"), null);
		eq(Std.parseInt("+-0x123"), null);
		eq(Std.parseInt("-+0x123"), null);
		eq(Std.parseInt("--0x123"), null);
		eq(Std.parseInt("+ 0x123"), null);
		eq(Std.parseInt("- 0x123"), null);
		// hexadecimal prefix with no number
		unspec(Std.parseInt.bind("0x"));
		unspec(Std.parseInt.bind("0x C"));
		unspec(Std.parseInt.bind("0x+A"));

		// parseFloat

		// general
		eq(Std.parseFloat("0"), 0.);
		eq(Std.parseFloat("0.0"), 0.);
		// preceeding zeroes
		eq(Std.parseFloat("0001"), 1.);
		eq(Std.parseFloat("0010"), 10.);
		// trailing text
		eq(Std.parseFloat("100x123"), 100.);
		eq(Std.parseFloat("12foo13"), 12.);
		feq(Std.parseFloat("5.3 "), 5.3);
		feq(Std.parseFloat("5.3 1"), 5.3);
		// signs
		feq(Std.parseFloat("123.45"), 123.45);
		feq(Std.parseFloat("+123.45"), 123.45);
		feq(Std.parseFloat("-123.45"), -123.45);
		// whitespace: space, horizontal tab, newline, vertical tab, form feed, and carriage return
		feq(Std.parseFloat("   5.2"), 5.2);
		feq(Std.parseFloat(" \t\n\x0b\x0c\r1.6"), 1.6);
		// whitespace and signs
		feq(Std.parseFloat('  	1.6'), 1.6);
		feq(Std.parseFloat('  	-1.6'), -1.6);
		feq(Std.parseFloat('  	+1.6'), 1.6);
		// exponent
		feq(Std.parseFloat("2.426670815e12"), 2.426670815e12);
		feq(Std.parseFloat("2.426670815E12"), 2.426670815e12);
		feq(Std.parseFloat("2.426670815e+12"), 2.426670815e+12);
		feq(Std.parseFloat("2.426670815E+12"), 2.426670815e+12);
		feq(Std.parseFloat("2.426670815e-12"), 2.426670815e-12);
		feq(Std.parseFloat("2.426670815E-12"), 2.426670815e-12);
		#if !interp
		eq(Std.parseFloat("6e"), 6);
		eq(Std.parseFloat("6E"), 6);
		#end
		// null
		t(Math.isNaN(Std.parseFloat(null)));
		// no number
		t(Math.isNaN(Std.parseFloat("")));
		t(Math.isNaN(Std.parseFloat("abcd")));
		t(Math.isNaN(Std.parseFloat("a10")));
		// invalid use of signs
		t(Math.isNaN(Std.parseFloat("++12.3")));
		t(Math.isNaN(Std.parseFloat("+-12.3")));
		t(Math.isNaN(Std.parseFloat("-+12.3")));
		t(Math.isNaN(Std.parseFloat("--12.3")));
		t(Math.isNaN(Std.parseFloat("+ 12.3")));
		t(Math.isNaN(Std.parseFloat("- 12.3")));

		// random
		var x = Std.random(2);
		t(x == 0 || x == 1);
		eq(Std.random(1), 0);
		eq(Std.random(0), 0);
		eq(Std.random(-100), 0);

	}
}

private enum SomeEnum<T> {
	NoArguments;
	OneArgument(t:T);
}
