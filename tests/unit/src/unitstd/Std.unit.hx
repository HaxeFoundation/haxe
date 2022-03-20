// is
var known:String = null;
(known is String) == false;

var unknown = null;
(unknown is String) == false;
(null is String) == false;
//("foo" is null) == false;

("" is String) == true;
(false is Bool) == true;
(1 is Int) == true;
(1.5 is Int) == false;
(1.5 is Float) == true;
([] is Array) == true;
(cast unit.MyEnum.A is Array) == false;

// isOfType
var known:String = null;
Std.isOfType(known, String) == false;

var unknown = null;
Std.isOfType(unknown, String) == false;
Std.isOfType(null, String) == false;
//Std.isOfType("foo", null) == false;

Std.isOfType("", String) == true;
Std.isOfType(false, Bool) == true;
Std.isOfType(1, Int) == true;
Std.isOfType(1.5, Int) == false;
Std.isOfType(1.5, Float) == true;
Std.isOfType([], Array) == true;
Std.isOfType(cast unit.MyEnum.A, Array) == false;

// instance
#if !js
Std.downcast("", String) == "";
#end
var a = [];
Std.downcast(a, Array) == a;
var parent:unit.MyClass.MyParent = new MyClass.MyChild1();
Std.downcast(parent, unit.MyClass.MyChild1) != null;
Std.downcast(null, Array) == null;
Std.downcast(null, String) == null;

var parent:unit.MyClass.IMyParent = new MyClass.MyChild1();
Std.downcast(parent, unit.MyClass.IMyChild) != null;

// string
var cwts = new ClassWithToString();
var cwtsc = new ClassWithToStringChild();
var cwtsc2 = new ClassWithToStringChild2();

Std.string(cwts) == "ClassWithToString.toString()";
Std.string(cwtsc) == "ClassWithToString.toString()";
Std.string(cwtsc2) == "ClassWithToStringChild2.toString()";

Std.string(SomeEnum.NoArguments) == "NoArguments";
Std.string(SomeEnum.OneArgument("foo")) == "OneArgument(foo)";

Std.string(null) == "null";

// int
Std.int(-1.7) == -1;
Std.int(-1.2) == -1;
Std.int(1.7) == 1;
Std.int(1.2) == 1;
Std.int(-0.7) == 0;
Std.int(-0.2) == 0;
Std.int(0.7) == 0;
Std.int(0.2) == 0;

// parseInt

// general
Std.parseInt("0") == 0;
Std.parseInt("-1") == -1;
// preceeding zeroes
Std.parseInt("0001") == 1;
Std.parseInt("0010") == 10;
// trailing text
Std.parseInt("100x123") == 100;
Std.parseInt("12foo13") == 12;
#if !php // https://github.com/HaxeFoundation/haxe/issues/10617
Std.parseInt("23e2") == 23;
#end
Std.parseInt("0x10z") == 16;
Std.parseInt("0x10x123") == 16;
// hexadecimals
Std.parseInt("0xff") == 255;
Std.parseInt("0x123") == 291;
Std.parseInt("0XFF") == 255;
Std.parseInt("0X123") == 291;
Std.parseInt("0X01") == 1;
Std.parseInt("0x01") == 1;
// signs
Std.parseInt("123") == 123;
Std.parseInt("+123") == 123;
Std.parseInt("-123") == -123;
Std.parseInt("0xa0") == 160;
Std.parseInt("+0xa0") == 160;
Std.parseInt("-0xa0") == -160;
// whitespace: space, horizontal tab, newline, vertical tab, form feed, and carriage return
Std.parseInt("   5") == 5;
Std.parseInt(" \t\n\x0b\x0c\r16") == 16;
Std.parseInt(" \t\n\x0b\x0c\r0xa") == 10;
// whitespace and signs
Std.parseInt('  	16') == 16;
Std.parseInt('  	-16') == -16;
Std.parseInt('  	+16') == 16;
Std.parseInt('  	0x10') == 16;
Std.parseInt('  	-0x10') == -16;
Std.parseInt('  	+0x10') == 16;
// binary and octal unsupported
Std.parseInt("010") == 10;
Std.parseInt("0b10") == 0;
// null
Std.parseInt(null) == null;
// no number
Std.parseInt("") == null;
Std.parseInt("abcd") == null;
Std.parseInt("a10") == null;
// invalid use of signs
Std.parseInt("++123") == null;
Std.parseInt("+-123") == null;
Std.parseInt("-+123") == null;
Std.parseInt("--123") == null;
Std.parseInt("+ 123") == null;
Std.parseInt("- 123") == null;
Std.parseInt("++0x123") == null;
Std.parseInt("+-0x123") == null;
Std.parseInt("-+0x123") == null;
Std.parseInt("--0x123") == null;
Std.parseInt("+ 0x123") == null;
Std.parseInt("- 0x123") == null;
// hexadecimal prefix with no number
unspec(Std.parseInt.bind("0x"));
unspec(Std.parseInt.bind("0x C"));
unspec(Std.parseInt.bind("0x+A"));

// parseFloat

// general
Std.parseFloat("0") == 0.;
Std.parseFloat("0.0") == 0.;
// preceeding zeroes
Std.parseFloat("0001") == 1.;
Std.parseFloat("0010") == 10.;
// trailing text
Std.parseFloat("100x123") == 100.;
Std.parseFloat("12foo13") == 12.;
Std.parseFloat("5.3 ") == 5.3;
Std.parseFloat("5.3 1") == 5.3;
// signs
Std.parseFloat("123.45") == 123.45;
Std.parseFloat("+123.45") == 123.45;
Std.parseFloat("-123.45") == -123.45;
// whitespace: space, horizontal tab, newline, vertical tab, form feed, and carriage return
Std.parseFloat("   5.2") == 5.2;
Std.parseFloat(" \t\n\x0b\x0c\r1.6") == 1.6;
// whitespace and signs
Std.parseFloat('  	1.6') == 1.6;
Std.parseFloat('  	-1.6') == -1.6;
Std.parseFloat('  	+1.6') == 1.6;
// exponent
Std.parseFloat("2.426670815e12") == 2.426670815e12;
Std.parseFloat("2.426670815E12") == 2.426670815e12;
Std.parseFloat("2.426670815e+12") == 2.426670815e+12;
Std.parseFloat("2.426670815E+12") == 2.426670815e+12;
Std.parseFloat("2.426670815e-12") == 2.426670815e-12;
Std.parseFloat("2.426670815E-12") == 2.426670815e-12;
#if !interp
Std.parseFloat("6e") == 6;
Std.parseFloat("6E") == 6;
#end
// null
Math.isNaN(Std.parseFloat(null)) == true;
// no number
Math.isNaN(Std.parseFloat("")) == true;
Math.isNaN(Std.parseFloat("abcd")) == true;
Math.isNaN(Std.parseFloat("a10")) == true;
// invalid use of signs
Math.isNaN(Std.parseFloat("++12.3")) == true;
Math.isNaN(Std.parseFloat("+-12.3")) == true;
Math.isNaN(Std.parseFloat("-+12.3")) == true;
Math.isNaN(Std.parseFloat("--12.3")) == true;
Math.isNaN(Std.parseFloat("+ 12.3")) == true;
Math.isNaN(Std.parseFloat("- 12.3")) == true;

// random
var x = Std.random(2);
x in [0,1];
Std.random(1) == 0;
Std.random(0) == 0;
Std.random(-100) == 0;
