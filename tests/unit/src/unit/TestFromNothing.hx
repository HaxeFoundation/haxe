package unit;

#if macro
import haxe.macro.Expr;
import haxe.macro.Context;
import haxe.macro.Type;
#end

private abstract Int2(Int) from Int to Int {}

private abstract Int3(Int) from Int to Int {}

@:forward
@:callable
@:arrayAccess
@:extern
private abstract Dep<T>(T) from T to T {

	@:fromNothing macro public static function fromNothing ():Expr {
		var unexpected = () -> Context.fatalError("unexpected", Context.currentPos());
		return switch Context.follow(Context.getExpectedType()) {
			case TAbstract(_.toString() => "unit._TestFromNothing.Dep", [t]):
				switch Context.follow(t) {
					case TAbstract(_.toString() => "Int", []): macro 1;
					case TAbstract(_.toString() => "unit._TestFromNothing.Int2", []): macro (2:Int2);
					case TAbstract(_.toString() => "unit._TestFromNothing.Int3", []): macro (3:Int3);
					case _: unexpected();
				}
			case t:
				unexpected();
		}
	}
}
#if !macro
class TestFromNothing extends Test {

	function test1() {
		function foo1 (?a:Dep<Int>) {
			return a;
		}

		t(foo1() == 1);
		t(foo1(2) == 2);

		function foo2 (?a:Dep<Int>, ?b:Dep<Int>) {
			return (a:Int)+(b:Int);
		}

		t(foo2() == 2);
		t(foo2(2,3) == 5);
		t(foo2(2) == 3);

		function foo2 (?a:Dep<Int>, ?b:Dep<Int2>) {
			return (a:Int) + (b:Int);
		}

		function foo3 (?a:Dep<Int>, ?b:Dep<Int2>, ?c:Dep<Int3>) {
			return (a:Int) + (b:Int) + (c:Int);
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
		t(foo2.bind(_,_)(7, 3) == 10);

		function foo4 <T>(x:T, plus:T->T->T, ?a:Dep<T>) {
			return plus(x, a);
		}

		t(foo4(1, (a, b) -> a + b) == 2);

		var f = foo4.bind(_, (a, b) -> a + b);
		t(f(1) == 2);

		t(foo4( (1:Int2), (a, b) -> (a:Int) + (b:Int) ) == 3);

		var f = foo4.bind(_, (a:Int2, b:Int2) -> ((a:Int) + (b:Int):Int2) );
		t(f(1) == 3);

		function foo5 <T>(?x:Dep<T>, ?y:Dep<T>) {
			return Std.string(x) + "-" + Std.string(y);
		}

		t(foo5(3) == "3-1");

	}
}
#end