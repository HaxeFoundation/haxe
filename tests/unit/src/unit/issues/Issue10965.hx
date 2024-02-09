package unit.issues;

class Issue10965 extends Test {
	#if jvm
	function testNeg() {
		var a:Single = 1;
		var c = -a;
		eq((-1 : Single), c);
	}

	function testIncPre() {
		var a:Single = 1;
		var c = ++a;
		eq((2 : Single), a);
		eq((2 : Single), c);
	}

	function testIncPost() {
		var a:Single = 1;
		var c = a++;
		eq((2 : Single), a);
		eq((1 : Single), c);
	}

	function testDecPre() {
		var a:Single = 1;
		var c = --a;
		eq((0 : Single), a);
		eq((0 : Single), c);
	}

	function testDecPost() {
		var a:Single = 1;
		var c = a--;
		eq((0 : Single), a);
		eq((1 : Single), c);
	}
	#end
}
