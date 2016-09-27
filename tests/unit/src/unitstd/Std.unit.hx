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

// instance
#if !js
Std.instance("", String) == "";
#end
var a = [];
Std.instance(a, Array) == a;
var parent:unit.MyClass.MyParent = new MyClass.MyChild1();
Std.instance(parent, unit.MyClass.MyChild1) != null;
Std.instance(null, Array) == null;
Std.instance(null, String) == null;

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
Std.parseInt("0") == 0;
Std.parseInt("   5") == 5;
Std.parseInt("0001") == 1;
Std.parseInt("0010") == 10;
Std.parseInt("100") == 100;
Std.parseInt("-100") == -100;
Std.parseInt("100x123") == 100;
Std.parseInt("12foo13") == 12;
Std.parseInt("") == null;
Std.parseInt("abcd") == null;
Std.parseInt("a10") == null;
Std.parseInt(null) == null;
Std.parseInt("0xFF") == 255;
Std.parseInt("0x123") == 291;
Std.parseInt("0XFF") == 255;
Std.parseInt("0X123") == 291;
Std.parseInt("0X01") == 1;
Std.parseInt("0x01") == 1;

// parseFloat
Std.parseFloat("0") == 0.;
Std.parseFloat("   5.3") == 5.3;
Std.parseFloat("0001") == 1.;
Std.parseFloat("100.45") == 100.45;
Std.parseFloat("-100.01") == -100.01;
Std.parseFloat("100x123") == 100.;
Math.isNaN(Std.parseFloat("")) == true;
Math.isNaN(Std.parseFloat("abcd")) == true;
Math.isNaN(Std.parseFloat("a10")) == true;
Math.isNaN(Std.parseFloat(null)) == true;
Std.parseFloat("5.3 ") == 5.3;
Std.parseFloat("0.0") == 0.;
Std.parseFloat("5.3 1") == 5.3;
Std.parseFloat("2.426670815e+12") == 2.426670815e+12;
Std.parseFloat("2.426670815E+12") == 2.426670815e+12;
Std.parseFloat("2.426670815e-12") == 2.426670815e-12;
Std.parseFloat("2.426670815E-12") == 2.426670815e-12;
// Std.parseInt("0x C") == 0;
// Std.parseInt("0x+A") == 0;

// random
var x = Std.random(2);
x in [0,1];
Std.random(1) == 0;
Std.random(0) == 0;
Std.random(-100) == 0;
