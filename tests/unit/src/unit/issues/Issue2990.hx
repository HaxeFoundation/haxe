package unit.issues;
import unit.Test;

class Issue2990 extends Test
{
	function test()
	{
		var u:UInt = 11;
		eq(typeof(u << 1), 'TAbstract(UInt,[])');
		eq(typeof(~u), 'TAbstract(UInt,[])');
		eq(typeof(u >> 1), 'TAbstract(UInt,[])');
		eq(typeof(u >>> 1), 'TAbstract(UInt,[])');
		eq(typeof(u + 1), 'TAbstract(UInt,[])');
		eq(typeof(u - 1), 'TAbstract(UInt,[])');
		eq(typeof(u / 2), 'TAbstract(Float,[])');
		eq(typeof(u * 2), 'TAbstract(UInt,[])');
		eq(typeof(u % 2), 'TAbstract(UInt,[])');
		eq(typeof(u % 2.1), 'TAbstract(Float,[])');
		eq(typeof(u * 2.1), 'TAbstract(Float,[])');
		eq(typeof(u / 2.1), 'TAbstract(Float,[])');
		eq(typeof(u - 2.1), 'TAbstract(Float,[])');
		eq(typeof(u + 2.1), 'TAbstract(Float,[])');

		eq(typeof(u > 2.1), 'TAbstract(Bool,[])');
		eq(typeof(u > 2), 'TAbstract(Bool,[])');
		eq(typeof(u > u), 'TAbstract(Bool,[])');
		eq(typeof(u >= 2.1), 'TAbstract(Bool,[])');
		eq(typeof(u >= 2), 'TAbstract(Bool,[])');
		eq(typeof(u < 2.1), 'TAbstract(Bool,[])');
		eq(typeof(u < 2), 'TAbstract(Bool,[])');
		eq(typeof(u < u), 'TAbstract(Bool,[])');
		eq(typeof(u <= 2.1), 'TAbstract(Bool,[])');
		eq(typeof(u <= 2), 'TAbstract(Bool,[])');

		eq(typeof(u == 2), 'TAbstract(Bool,[])');
		eq(typeof(u == 2.1), 'TAbstract(Bool,[])');
		eq(typeof(u == u), 'TAbstract(Bool,[])');
		eq(typeof(u != 2), 'TAbstract(Bool,[])');
		eq(typeof(u != 2.1), 'TAbstract(Bool,[])');
		eq(typeof(u != u), 'TAbstract(Bool,[])');

		eq(5.5, 11 / 2);
	}

	macro public static function typeof(expr:haxe.macro.Expr):haxe.macro.Expr
	{
		return haxe.macro.Context.makeExpr( haxe.macro.Context.typeof(expr) + '', expr.pos );
	}
}
