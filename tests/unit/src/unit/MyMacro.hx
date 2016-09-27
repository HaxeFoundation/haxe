package unit;

import haxe.macro.Expr;
import haxe.macro.Context;

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

class MyMacroHelper
{
	static public macro function followWithAbstracts(e1:Expr):Expr
	{
		return macro $v{Context.followWithAbstracts(Context.typeof(e1)) + ""};
	}

	static public macro function followWithAbstractsOnce(e1:Expr):Expr
	{
		return macro $v{Context.followWithAbstracts(Context.typeof(e1),true) + ""};
	}

	static public macro function ttype(e1:Expr):Expr
	{
		return macro $v{Context.typeof(e1) + ""};
	}
}
