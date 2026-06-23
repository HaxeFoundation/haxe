package unit;

#if macro
import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.Type;
#end

class TestImplicitArgResolver extends Test {
	// a plain (non-macro) resolver fills an omitted optional argument
	function testPlain() {
		eq("plain", plain());
		eq("given", plain("given"));
	}

	// a resolver may itself take ?pos:PosInfos, which forwards the original call site
	function testPosForwarding() {
		eq("testPosForwarding", withPos());
	}

	// a macro resolver runs at the call site (here: capturing the enclosing method)
	function testMacro() {
		eq("testMacro", withMacro());
	}

	// several implicit arguments (resolver + PosInfos) all fill independently
	function testMultiple() {
		eq("plain/testMultiple", multi());
		eq("plain/plain", two());
	}

	// a resolver-typed optional argument may sit before a rest argument
	function testBeforeRest() {
		eq("plain|1,2,3", beforeRest(1, 2, 3));
		eq("plain|", beforeRest());
	}

	// a macro resolver may inspect the expected type, so a generic abstract can
	// resolve a different value per type parameter (ported from #6616)
	function testGenericResolver() {
		function foo1(?a:Dep<Int>) {
			return a;
		}

		t(foo1() == 1);
		t(foo1(2) == 2);

		function foo2(?a:Dep<Int>, ?b:Dep<Int>) {
			return (a : Int) + (b : Int);
		}

		t(foo2() == 2);
		t(foo2(2, 3) == 5);
		t(foo2(2) == 3);

		function foo2(?a:Dep<Int>, ?b:Dep<Int2>) {
			return (a : Int) + (b : Int);
		}

		function foo3(?a:Dep<Int>, ?b:Dep<Int2>, ?c:Dep<Int3>) {
			return (a : Int) + (b : Int) + (c : Int);
		}

		t(foo2() == 3);
		t(foo3() == 6);

		t(foo2(7) == 9);
		t(foo3(7) == 12);

		t(foo2.bind()() == 3);
		t(foo3.bind()() == 6);

		t(foo2.bind(7)() == 9);
		t(foo3.bind(7)() == 12);

		t(foo2.bind(_)(7) == 9);
		t(foo2.bind(_, _)(7, 3) == 10);

		function foo4<T>(x:T, plus:T->T->T, ?a:Dep<T>) {
			return plus(x, a);
		}

		t(foo4(1, (a, b) -> a + b) == 2);

		var f = foo4.bind(_, (a, b) -> a + b);
		t(f(1) == 2);

		t(foo4((1 : Int2), (a, b) -> (a : Int) + (b : Int)) == 3);

		var f = foo4.bind(_, (a:Int2, b:Int2) -> ((a : Int) + (b : Int) : Int2));
		t(f(1) == 3);

		function foo5<T>(?x:Dep<T>, ?y:Dep<T>) {
			return Std.string(x) + "-" + Std.string(y);
		}

		t(foo5(3) == "3-1");

		function foo(?x:Int = 5, ?y:Dep<Int>) {
			return Std.string(x) + "-" + Std.string(y);
		}

		t(foo() == "5-1");
		t(foo.bind()() == "5-1");

		function foo(?x:Dep<Int>, ?y:Int = 5) {
			return Std.string(x) + "-" + Std.string(y);
		}

		t(foo() == "1-5");
		t(foo.bind()() == "1-5");

		function foo(?w:Int = 7, ?x:Dep<Int>, ?y:Int = 5) {
			return Std.string(w) + "-" + Std.string(x) + "-" + Std.string(y);
		}

		t(foo() == "7-1-5");
		t(foo.bind()() == "7-1-5");

		function foo(?x:Int = 5, ?y:haxe.PosInfos) {
			return Std.string(x) + "-" + (y != null);
		}

		t(foo.bind()() == "5-true");
	}

	static function plain(?c:Plain):String
		return (c : String);

	static function withPos(?c:PosCtx):String
		return (c : String);

	static function withMacro(?c:MacroCtx):String
		return (c : String);

	static function multi(?c:Plain, ?pos:haxe.PosInfos):String
		return (c : String) + "/" + pos.methodName;

	static function two(?a:Plain, ?b:Plain):String
		return (a : String) + "/" + (b : String);

	static function beforeRest(?c:Plain, ...rest:Int):String
		return (c : String) + "|" + rest.toArray().join(",");
}

@:implicitArgResolver(resolve)
private abstract Plain(String) from String to String {
	public inline function new(s:String)
		this = s;

	static function resolve():Plain
		return new Plain("plain");
}

@:implicitArgResolver(resolve)
private abstract PosCtx(String) to String {
	public inline function new(s:String)
		this = s;

	static function resolve(?pos:haxe.PosInfos):PosCtx
		return new PosCtx(pos.methodName);
}

@:implicitArgResolver(resolve)
private abstract MacroCtx(String) from String to String {
	macro static function resolve() {
		return macro $v{haxe.macro.Context.getLocalMethod()};
	}
}

@:implicitArgResolver(resolve)
private abstract Dep<T>(T) to T {
	inline function new(x:T)
		this = x;

	@:from public static inline function fromT<T>(t:T):Dep<T>
		return new Dep(t);

	// the resolver inspects the expected type to pick a value per type parameter
	macro static function resolve():Expr {
		var unexpected = () -> Context.fatalError("unexpected", Context.currentPos());
		return switch Context.follow(Context.getExpectedType()) {
			case TAbstract(_.toString() => "unit._TestImplicitArgResolver.Dep", [t]):
				switch Context.follow(t) {
					case TAbstract(_.toString() => "Int", []): macro 1;
					case TAbstract(_.toString() => "unit._TestImplicitArgResolver.Int2", []): macro(2 : Int2);
					case TAbstract(_.toString() => "unit._TestImplicitArgResolver.Int3", []): macro(3 : Int3);
					case _: unexpected();
				}
			case _: unexpected();
		}
	}
}

@:transitive
private abstract Int2(Int) from Int to Int {}

@:transitive
private abstract Int3(Int) from Int to Int {}
