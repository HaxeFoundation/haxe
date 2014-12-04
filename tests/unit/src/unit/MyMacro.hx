package unit;

import haxe.macro.Expr;

class MyRestMacro {
	static public macro function testRest1(e:Expr, r:Array<Expr>) {
		var ret = [e];
		for (e in r)
			ret.push(e);
		return macro $a{ret};
	}

	static public macro function testRest2(e1:Expr, e2:Expr, r:Array<Expr>) {
		var ret = [e1,e2];
		for (e in r)
			ret.push(e);
		return macro $a{ret};
	}
}