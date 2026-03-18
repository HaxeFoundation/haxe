package unit.issues;
import unit.Test;

class Issue2990 extends Test
{
function test()
{
var u:haxe.UInt32 = 11;
// UInt32 operations return UInt32 (integer type, no Float mixing)
eq(typeof(u << 1), 'TAbstract(haxe.UInt32,[])');
eq(typeof(~u), 'TAbstract(haxe.UInt32,[])');
eq(typeof(u >> 1), 'TAbstract(haxe.UInt32,[])');
eq(typeof(u >>> 1), 'TAbstract(haxe.UInt32,[])');
eq(typeof(u + 1), 'TAbstract(haxe.UInt32,[])');
eq(typeof(u - 1), 'TAbstract(haxe.UInt32,[])');
// Division returns UInt32 (integer division), not Float
eq(typeof(u / cast(2, haxe.UInt32)), 'TAbstract(haxe.UInt32,[])');
eq(typeof(u * cast(2, haxe.UInt32)), 'TAbstract(haxe.UInt32,[])');
eq(typeof(u % cast(2, haxe.UInt32)), 'TAbstract(haxe.UInt32,[])');

eq(typeof(u > u), 'TAbstract(Bool,[])');
eq(typeof(u >= u), 'TAbstract(Bool,[])');
eq(typeof(u < u), 'TAbstract(Bool,[])');
eq(typeof(u <= u), 'TAbstract(Bool,[])');
eq(typeof(u == u), 'TAbstract(Bool,[])');
eq(typeof(u != u), 'TAbstract(Bool,[])');

eq(5.5, 11 / 2);
}

macro public static function typeof(expr:haxe.macro.Expr):haxe.macro.Expr
{
return haxe.macro.Context.makeExpr( haxe.macro.Context.typeof(expr) + '', expr.pos );
}
}
