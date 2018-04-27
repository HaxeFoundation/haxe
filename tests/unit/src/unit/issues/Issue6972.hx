package unit.issues;

import unit.HelperMacros.isNullable as n;

class Issue6972 extends unit.Test {
	function test() {
		f1(1);
		f2(1);
		f3();
		f4();
		f5();
		f6();
		f7();
		f8();

		f11();
		f12();
		f13();
		f14();
		f15();
		f16();
		f17();
		f18();
	}

	function f1(x:Int) f(n(x));
	function f2(x:Null<Int>) t(n(x));
	function f3(x = null) t(n(x));
	function f4(x = 1) f(n(x));
	function f5(x:Int = null) t(n(x));
	function f6(x:Int = 1) f(n(x));
	function f7(x:Null<Int> = 1) t(n(x));
	function f8(x:Null<Int> = null) t(n(x));

	function f11(?x:Int) t(n(x));
	function f12(?x:Null<Int>) t(n(x));
	function f13(?x = null) t(n(x));
	function f14(?x = 1) t(n(x));
	function f15(?x:Int = null) t(n(x));
	function f16(?x:Int = 1) t(n(x));
	function f17(?x:Null<Int> = 1) t(n(x));
	function f18(?x:Null<Int> = null) t(n(x));
}