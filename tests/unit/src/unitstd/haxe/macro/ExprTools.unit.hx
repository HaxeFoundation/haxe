var econst = macro 1;
var econtinue = macro continue;
var ebreak = macro break;
var efield = macro "foo".length;
var eparenthesis = macro (1);
var euntyped = macro untyped 1;
var ethrow = macro throw 1;
var eunop = macro -1;
var ecast = macro cast 1;
var emeta = macro @myMeta 1;
var earray = macro 1[0];
var ewhile1 = macro while (1) "foo";
var ewhile2 = macro do "foo" while (1);
var ebinop = macro 1 + 1;
var efor = macro for (1) "foo";
var ein = macro i in 1;
var evars = macro var x = 1, y = 2;
var etry = macro try 1 catch (e:Dynamic) "foo" catch(e2:String) "bar";
var eternary = macro 1 ? 2 : 3;
var earraydecl = macro [1, 2];
var enew = macro new String(1, 2);
var eblock = macro { 1; 2; };
var eobjectdecl = macro { foo: 1, bar: 2 };
var ecall = macro foo(1, 2);
var ereturn = macro return 1;
var efunction = macro function n(x = 1, y = 2) 3;
var eswitch = macro switch 1 { case 2,3: case 4 if (5): case 6: };

// iter
var subject = macro {
	var p = new neko.io.Process("java", ["-jar", neko.Web.getCwd() + "/java/java.jar"]);
	if( neko.Web.isModNeko )
		neko.Web.setHeader("Content-Type","text/plain");
	try {
		while( true ) {
			var c = p.stdout.readByte();
			neko.Lib.print(StringTools.htmlEscape(String.fromCharCode(c)));
		}
	} catch ( e : haxe.io.Eof ) {
	}
	neko.Lib.print(StringTools.htmlEscape(p.stderr.readAll().toString()));
};

var strings = [];
var upperIdents = [];

function extract(e) {
	switch(e.expr) {
		case haxe.macro.Expr.ExprDef.EConst(haxe.macro.Expr.Constant.CString(s,_)):
			strings.push(s);
		case haxe.macro.Expr.ExprDef.EConst(haxe.macro.Expr.Constant.CIdent(s)) if (s.charCodeAt(0) >= 'A'.code && s.charCodeAt(0) <= 'Z'.code):
			upperIdents.push(s);
		case _:
			haxe.macro.ExprTools.iter(e, extract);
	}
}
extract(subject);
strings == ["java", "-jar", "/java/java.jar", "Content-Type", "text/plain"];
upperIdents == ["StringTools", "String", "StringTools"];

var iter = haxe.macro.ExprTools.iter;
var fail = function(e) throw "I was called";
function seq(s, e) eq(s, Std.string(e.expr));
function check(e, exp, ?pos) {
	function f(e2) {
		eq(Std.string(e2.expr), exp.shift(), pos);
	}
	iter(e, f);
}

iter(econst, fail);
iter(econtinue, fail);
iter(ebreak, fail);
iter(efield, seq.bind("EConst(CString(foo,Double))"));
iter(eparenthesis, seq.bind("EConst(CInt(1))"));
iter(euntyped, seq.bind("EConst(CInt(1))"));
iter(ethrow, seq.bind("EConst(CInt(1))"));
iter(eunop, seq.bind("EConst(CInt(1))"));
iter(ecast, seq.bind("EConst(CInt(1))"));
iter(emeta, seq.bind("EConst(CInt(1))"));
check(earray, ["EConst(CInt(1))", "EConst(CInt(0))"]);
check(ewhile1, ["EConst(CInt(1))", "EConst(CString(foo,Double))"]);
check(ewhile2, ["EConst(CInt(1))", "EConst(CString(foo,Double))"]);
check(ebinop, ["EConst(CInt(1))", "EConst(CInt(1))"]);
check(efor, ["EConst(CInt(1))", "EConst(CString(foo,Double))"]);
check(ein, ["EConst(CIdent(i))", "EConst(CInt(1))"]);
check(evars, ["EConst(CInt(1))", "EConst(CInt(2))"]);
check(etry, ["EConst(CInt(1))", "EConst(CString(foo,Double))", "EConst(CString(bar,Double))"]);
check(eternary, ["EConst(CInt(1))", "EConst(CInt(2))", "EConst(CInt(3))"]);
check(earraydecl, ["EConst(CInt(1))", "EConst(CInt(2))"]);
check(enew, ["EConst(CInt(1))", "EConst(CInt(2))"]);
check(eblock, ["EConst(CInt(1))", "EConst(CInt(2))"]);
check(eobjectdecl, ["EConst(CInt(1))", "EConst(CInt(2))"]);
check(ecall, ["EConst(CIdent(foo))", "EConst(CInt(1))", "EConst(CInt(2))"]);
check(ereturn, ["EConst(CInt(1))"]);
check(efunction, ["EConst(CInt(1))", "EConst(CInt(2))", "EConst(CInt(3))"]);
check(eswitch, ["EConst(CInt(1))", "EConst(CInt(2))", "EConst(CInt(3))", "EConst(CInt(4))", "EConst(CInt(5))", "EConst(CInt(6))"]);

// map
function wrap(e) return macro ($e);
function unwrap(e) return switch(e.expr) {
	case haxe.macro.Expr.ExprDef.EParenthesis(e): e;
	case _: e;
}
var map = haxe.macro.ExprTools.map;
function check(e, ?pos) {
	var e2 = map(e, wrap);
	var e3 = map(e, unwrap);
	eq(Std.string(e.expr), Std.string(e3.expr), pos);
}
map(econst, wrap).expr == econst.expr;
map(econtinue, wrap).expr == econtinue.expr;
map(ebreak, wrap).expr == ebreak.expr;
check(efield);
check(eparenthesis);
check(euntyped);
check(ethrow);
check(eunop);
check(ecast);
check(emeta);
check(earray);
check(ewhile1);
check(ewhile2);
check(ebinop);
check(efor);
check(ein);
check(evars);
check(etry);
check(eternary);
check(earraydecl);
check(enew);
check(eblock);
check(eobjectdecl);
check(ecall);
// we can check these once ExprTools.toString has been added
//check(efunction);
//check(eswitch);
check(ereturn);

// getValue
var getValue = haxe.macro.ExprTools.getValue;

1 == getValue(macro 1);
"1" == getValue(macro "1");
12.2 == getValue(macro 12.2);
true == getValue(macro true);
false == getValue(macro false);
null == getValue(macro null);
null == getValue(macro (null));
null == getValue(macro untyped null);
null == getValue(macro @:meta null);

var obj = getValue(macro { f1: 1, f2: "foo" });
1 == obj.f1;
"foo" == obj.f2;

var a:Array<Dynamic> = getValue(macro [1, "foo", true]);
1 == a[0];
"foo" == a[1];
true == a[2];

-1 == getValue(macro -1);
false == getValue(macro !true);
true == getValue(macro !false);

5 == getValue(macro 2 + 3);
-1 == getValue(macro 2 - 3);
6 == getValue(macro 2 * 3);
2 == getValue(macro 4 / 2);
1 == getValue(macro 3 % 2);
true == getValue(macro 1 == 1);
false == getValue(macro 1 != 1);
true == getValue(macro 1 > 0);
false == getValue(macro 0 > 1);
true == getValue(macro 0 < 1);
false == getValue(macro 1 < 0);
true == getValue(macro 1 >= 0);
true == getValue(macro 1 >= 1);
false == getValue(macro 0 >= 1);
true == getValue(macro 0 <= 1);
true == getValue(macro 1 <= 1);
false == getValue(macro 1 <= 0);
7 == getValue(macro 5 | 2);
1 == getValue(macro 5 & 3);
6 == getValue(macro 5 ^ 3);
true == getValue(macro true && true);
false == getValue(macro true && false);
true == getValue(macro true || false);
false == getValue(macro false || false);
8 == getValue(macro 4 << 1);
4 == getValue(macro 8 >> 1);
4 == getValue(macro 8 >>> 1);