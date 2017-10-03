package unit;
import haxe.ds.Option;
import haxe.macro.Expr;
import unit.HelperMacros.getErrorMessage;

using unit.TestMatch;

enum Tree<T> {
	Leaf(t:T);
	Node(l:Tree<T>, r:Tree<T>);
}

enum A<T> {
	TA<Q>(q : Q) : A<Q>;
	TB(v : Bool) : A<Bool>;
	TC(v : Bool) : A<String>;
}

enum X<A> {
	U1(x:Int):X<Int>;
	U2:X<String>;
}

enum NE {
	A(?x:Int);
}

enum MiniType {
	MTString(t:MiniRef<String>, tl:Array<MiniType>);
	MTInt(t:MiniRef<Int>, tl:Array<MiniType>);
}

typedef MiniRef<T> = {
	public function get():T;
}

class TestMatch extends Test {

	static function switchNormal(e:Expr):String {
		return switch(e.expr) {
			case EConst(CString(s,_)): s;
			case EParenthesis( { expr : EConst(CString(s,_)) } )
			| EUntyped( { expr : EConst(CString(s,_)) } ):
				s;
			case EField(_, s):
				s;
			case EArray(_, { expr : EConst(CInt(i) | CFloat(i)) } ):
				Std.string(i);
			case EBinop(OpIn, _, { expr : e, pos : _ }) :
				Std.string(e);
			case _:
				"not_found";
		}
	}

	static function switchCapture(e:Expr) {
		return switch(e) {
			case { expr : EConst(const = (CString("foobar",_) | CInt("9"))) } :
				const;
			case _:
				null;
		}
	}

	static function switchArray(e:Expr):String {
		return switch(e.expr) {
			case EArrayDecl([]):
				"[]";
			case EArrayDecl([a]):
				"[" + Std.string(a.expr) + "]";
			case EArrayDecl([a,b]):
				"[" + Std.string(a.expr) + "," + Std.string(b.expr) + "]";
			case _:
				"_";
		}
	}

	static function switchArray2(a:Array<String>):String {
		return switch(a) {
			case ["a", "b"]: "0";
			case ["a"]: "1";
			case ["b"]: "2";
			case [a]: "3:" + a;
			case [a, b]: "4:" + a + "," +b;
			case var a if (a.length == 3): "5:" + a.length;
			case []: "6";
			case _: "7";
		}
	}

	static function switchStructure(a: { foo:String, bar:String } ) {
		return switch(a) {
			case { foo: "val1", bar:"val2" } : "0";
			case { foo: "val1" } : "1";
			case { bar: "val2" } : "2";
			case { bar: a } : a;
		}
	}

	static function switchCrazy(e:Expr) {
		return switch(e.expr) {
			case EUntyped( { expr : EParenthesis( { expr : EArray( { expr: a = EConst(CString(_)) }, { expr : EConst(CInt(b)) } ) } ) } ):
				Std.string(a) + ":" +b;
			case _:
				"_";
		}
	}

	static function switchGuard(e:Expr):String {
		return switch(e.expr) {
			case EConst(CString(s,_)) if (StringTools.startsWith(s, "foo")):
				"1";
			case EConst(CString(s,_)) if (StringTools.startsWith(s, "bar")):
				"2";
			case EConst(CInt(i)) if (switch(Std.parseInt(i) * 2) { case 4: true; case _: false; }):
				"3";
			case EConst(_):
				"4";
			case _:
				"5";
		}
	}

	static function switchClass<T>(cl:Class<T>) {
		return switch(cl) {
			case String: "String";
			case MyClass: "unit.MyClass";
			case var a: "other: " +Type.getClassName(a);
		}
	}

	function testBasic() {
		eq("bar", switchNormal(macro "bar"));
		eq("bar", switchNormal(macro ("bar")));
		eq("bar", switchNormal(macro untyped "bar"));
		eq("foo", switchNormal(macro null.foo));
		eq("22", switchNormal(macro null[22]));
		eq("22.5", switchNormal(macro null[22.5]));
		eq("EConst(CInt(0))", switchNormal(macro 1 in 0));
		eq("not_found", switchNormal(macro null["22"]));

		t(null != switchCapture(macro "foobar"));
		t(null == switchCapture(macro "fooba"));
		t(null != switchCapture(macro 9));
		t(null == switchCapture(macro 10));

		eq("[]", switchArray(macro []));
		eq("_", switchArray(macro 2));
		eq("[EConst(CInt(22))]", switchArray(macro [22]));
		eq("[EConst(CInt(22)),EConst(CString(foo,Double))]", switchArray(macro [22,"foo"]));
		eq("_", switchArray(macro [22, "foo", "bar"]));

		eq("0", switchArray2(["a", "b"]));
		eq("1", switchArray2(["a"]));
		eq("2", switchArray2(["b"]));
		eq("3:c", switchArray2(["c"]));
		eq("4:a,a", switchArray2(["a","a"]));
		eq("4:b,a", switchArray2(["b","a"]));
		eq("5:3", switchArray2(["a","a","a"]));
		eq("6", switchArray2([]));
		eq("7", switchArray2(["a", "a", "a", "b"]));

		eq("EConst(CString(foobar,Double)):12", switchCrazy(macro untyped ("foobar"[12])));

		eq("1", switchGuard(macro "foobar"));
		eq("2", switchGuard(macro "barfoo"));
		eq("3", switchGuard(macro 2));
		eq("4", switchGuard(macro 5));
		eq("4", switchGuard(macro "bazfoo"));
		eq("5", switchGuard(macro []));

		eq("0", switch ([true, 1, "foo"]) {
			case [true, 1, "foo"]: "0";
			case [true, 1, _]: "1";
			case _: "_";
		});

		eq("0", switch [true, 1, "foo"] {
			case [true, 1, "foo"]: "0";
			case [true, 1, _]: "1";
			case _: "_";
		});

		eq("1", switch [true, 1, "bar"] {
			case [true, 1, "foo"]: "0";
			case [true, 1, _]: "1";
			case _: "_";
		});

		eq("_", switch [false, 1, "foo"] {
			case [true, 1, "foo"]: "0";
			case [true, 1, _]: "1";
			case _: "_";
		});

		eq("1", switch [1, 2] {
			case [0, 0] | [1, 2]: "1";
			case [1, 1]: "2";
			case _: "_";
		});

		var t = TA("foo");
		eq("0", switch(t) {
			case TA("foo"): "0";
			case TA(_): "1";
			case TC(_): "2";
		});
	}

	function testTuple() {
		function test(a:Int, b:Int, c:Int) return switch [a, b, c] {
			case [x, 1, 2] | [1, 2, x] | [1, x, 2]: '0|x:$x';
			case [3, 4, z] | [z, 3, 4] | [3, z, 4]: '1|z:$z';
			case [1, y, z] | [2, z, y]: '2|y:$y,z:$z';
			case [x, y, z]: '_:x:$x,y:$y,z:$z';
		}
		eq("0|x:9", test(9, 1, 2));
		eq("0|x:9", test(1, 2, 9));
		eq("0|x:9", test(1, 9, 2));
		eq("1|z:12", test(3, 4, 12));
		eq("1|z:12", test(12, 3, 4));
		eq("1|z:12", test(3, 12, 4));
		eq("2|y:9,z:8", test(1, 9, 8));
		eq("2|y:9,z:8", test(2, 8, 9));
		eq("_:x:9,y:8,z:7", test(9, 8, 7));
	}

	function testGrouping() {
		function test(v) return switch(v) {
			case 1, 2, 3: "0";
			case val = (4 | 5 | 6) if (val == 5): "1";
			case 4, 5, 6: "2";
			case 8, 9: "3";
			case var x: '_:$x';
		}
		var results = ["_:0", "0", "0", "0", "2", "1", "2", "_:7", "3", "3", "_:10"];
		for (i in 0...results.length) {
			eq(results[i], test(i));
		}
	}

	function testSubtyping() {
		var c = new MyClass.InitBase();
		var r = switch(c) {
			case { s: "foo" } :
				"s = foo";
			case _:
				"_";
		}
		eq("s = foo", r);

		eq("0", switchStructure( { foo:"val1", bar:"val2" } ));
		eq("1", switchStructure( { foo:"val1", bar:"val1" } ));
		eq("2", switchStructure( { foo:"val2", bar:"val2" } ));
		eq("val1", switchStructure( { foo:"val2", bar:"val1" } ));
	}

	public static function toStringX<Z>(x1:X<Z>) {
		return switch (x1) {
			case U1(x) if (x > 1): ">1";
			case U1(x) if (x <= 1): "<=1";
			case U1(_): throw "this is impossible to reach actually";
			case U2: "U2";
		}
	}

	function testGadt() {
		eq("<=1", toStringX(U1(1)));
		eq(">1", toStringX(U1(2)));
		eq("U2", toStringX(U2));
	}

	function testClassSwitch() {
		eq("String", switchClass(String));
		eq("unit.MyClass", switchClass(MyClass));
		eq("other: unit.TestMatch", switchClass(TestMatch));
	}

	function testOr() {
		var i1 = macro 1;
		var i2 = macro 2;
		var f1 = macro 3.9;
		var f2 = macro 4.8;
		eq("11", orMatch(i1, i1));
		eq("12", orMatch(i1, i2));
		eq("13.9", orMatch(i1, f1));
		eq("14.8", orMatch(i1, f2));

		eq("21", orMatch(i2, i1));
		eq("22", orMatch(i2, i2));
		eq("23.9", orMatch(i2, f1));
		eq("24.8", orMatch(i2, f2));

		eq("3.91", orMatch(f1, i1));
		eq("3.92", orMatch(f1, i2));
		eq("3.93.9", orMatch(f1, f1));
		eq("3.94.8", orMatch(f1, f2));

		eq("4.81", orMatch(f2, i1));
		eq("4.82", orMatch(f2, i2));
		eq("4.83.9", orMatch(f2, f1));
		eq("4.84.8", orMatch(f2, f2));
	}

	function testStaticNull() {
		var v = A();
		var r = switch(v) {
			case A(x):
				if (x == null) "null";
				else "not null";
		}
		eq("null", r);
	}

	static function orMatch(e1, e2) {
		return switch([e1.expr, e2.expr]) {
			case [EConst(CFloat(a) | CInt(a)), EConst(CFloat(b) | CInt(b))]: a + b;
			case _: null;
		}
	}

	function testNonExhaustiveness() {
		eq("Unmatched patterns: false", getErrorMessage(switch(true) {
			case true:
		}));
		eq("Unmatched patterns: OpNeg | OpNegBits", getErrorMessage(switch(OpIncrement) {
			case OpIncrement:
			case OpDecrement:
			case OpNot:
		}));
		eq("Unmatched patterns: Node(Node, _)", getErrorMessage(switch(Leaf("foo")) {
			case Node(Leaf("foo"), _):
			case Leaf(_):
		}));
		eq("Unmatched patterns: Leaf", getErrorMessage(switch(Leaf("foo")) {
			case Node(_, _):
			case Leaf(_) if (false):
		}));
		eq("Unmatched patterns: Leaf(_)", getErrorMessage(switch(Leaf("foo")) {
			case Node(_, _):
			case Leaf("foo"):
		}));
		eq("Unmatched patterns: false", getErrorMessage(switch [1, true, "foo"] {
			case [_, true, _]:
		}));
		//var x:Null<Bool> = true;
		//eq("Unmatched patterns: null", getErrorMessage(switch x {
			//case true:
			//case false:
		//}));
		//var t:Null<Tree<String>> = null;
		//eq("Unmatched patterns: null", getErrorMessage(switch t {
			//case Leaf(_):
			//case Node(_):
		//}));
	}

	function testInvalidBinding() {
		eq("Variable y must appear exactly once in each sub-pattern", getErrorMessage(switch(Leaf("foo")) {
			case Leaf(x) | Leaf(y):
		}));
		eq("Variable y must appear exactly once in each sub-pattern", getErrorMessage(switch(Leaf("foo")) {
			case Leaf(x) | Leaf(x) | Leaf(y):
		}));
		eq("Variable x must appear exactly once in each sub-pattern", getErrorMessage(switch(Leaf("foo")) {
			case Leaf(x) | Leaf(x) | Leaf(_):
		}));
		eq("Variable l must appear exactly once in each sub-pattern", getErrorMessage(switch(Leaf("foo")) {
			case Node(l = Leaf(x),_) | Node(Leaf(x), _):
		}));
		eq("Variable l is bound multiple times", getErrorMessage(switch(Leaf("foo")) {
			case Node(l = Leaf(l), _):
		}));
		eq("String should be unit.Tree<String>", getErrorMessage(switch(Leaf("foo")) {
			case Node(l = Leaf(_), _) | Leaf(l):
			case _:
		}));
	}

	function testNullPattern() {
		var i:Null<Int> = null;
		var r = switch(i) {
			case 1: 1;
			case null: 2;
			case _: 3;
		}
		eq(2, r);

		// this should not compile because the argument is not explicitly Null
		//var e = EConst(null);
		//var r = switch(e) {
			//case EConst(null): 1;
			//case _: 2;
		//}

		var t:Null<Tree<String>> = null;
		var r = switch(t) {
			case Leaf(_): 1;
			case null if (i != null): 2;
			case null: 3;
			case Node(_): 4;
		}
		eq(r, 3);

		var e1 = macro if (1) 2;
		var e2 = macro if (1) 2 else 3;
		function matchIf(e) {
			return switch(e.expr) {
				case EIf(_, _, null): 1;
				case EIf(_, _, _): 2;
				case _: 3;
			}
		}
		eq(1, matchIf(e1));
		eq(2, matchIf(e2));

		var t = Leaf("foo");
		function f(t) return switch(t) {
			case Leaf(null): "null";
			case Leaf(e): e;
			case Node(_): "default";
		}
		eq(f(t), "foo");

		function f(a) {
			return switch(a:{a: Int}) {
				case {a: 1}: 1;
				case null: 2;
				default: 3;
			}
		}

		eq(f(null), 2);
		eq(f({a: 1}), 1);
		eq(f({a: 2}), 3);
	}

	function testFakeEnumAbstract() {
		#if !macro
		var a = unit.MyAbstract.FakeEnumAbstract.NotFound;
		var r = switch(a) {
			case NotFound: 1;
			case _: 2;
		}
		eq(r, 1);

		eq("Unmatched patterns: MethodNotAllowed", getErrorMessage(switch(a) {
			case NotFound:
		}));
		#end
	}

	function testExtractors() {
		function f(i) {
			return switch(i) {
				case 1,2,3: 1;
				case _.even() => true: 2;
				case 4: throw "unreachable";
				case _: 3;
			}
		}

		eq(1, f(1));
		eq(1, f(2));
		eq(1, f(3));
		eq(2, f(4));
		eq(3, f(5));
		eq(3, f(7));
		eq(3, f(9));
		eq(2, f(6));
		eq(2, f(8));

		function ref<T>(t:T):MiniRef<T> return {
			get: function() return t
		}

		function f(t:MiniType) {
			return switch (t) {
				case MTString(_.deref() => "Foo", []): "Foo";
				case MTString(_.deref() => "Bar" | "Baz", _): "BarBaz";
				case MTInt(_.deref() => i, []): 'Int:$i';
				case MTString(_): "OtherString";
				case _: "Other";
			}
		}

		eq("Foo", f(MTString(ref("Foo"), [])));
		eq("BarBaz", f(MTString(ref("Bar"), [])));
		eq("BarBaz", f(MTString(ref("Baz"), [])));
		eq("OtherString", f(MTString(ref("a"), [])));
		eq("OtherString", f(MTString(ref(""), [])));
		eq("Int:12", f(MTInt(ref(12), [])));
		eq("Other", f(MTInt(ref(12), [MTInt(ref(10),[])])));

		function g(i : Array<Int>) {
			return switch(i) {
				case [x]: 1;
				case isPair(_) => Some(p) : p.a+p.b;
				case var arr: 3;
			}
		}

		eq(3, g([]));
		eq(1, g([1]));
		eq(5, g([2, 3]));
		eq(3, g([2, 3, 4]));

		var anon = {
			odd: function(i) return i & 1 != 0
		};

		var i = 9;
		var r = switch(i) {
			case 1: 1;
			case anon.odd(_) => true: 2;
			case 9: 3;
			case _: 4;
		}
		eq(2, r);

		function mul(i1,i2) return i1 * i2;

		function check(i) {
			return switch(i) {
				case 1: 1;
				case mul(_, 4) => 8: 2;
				case mul(_, 5) => 15: 3;
				case _: 4;
			}
		}

		eq(1, check(1));
		eq(2, check(2));
		eq(3, check(3));
		eq(4, check(4));

		function is<T>(pred : T -> Bool) return function (x : T) {
			return pred(x)?Some(x):None;
		}

		function isNot<T>(pred : T -> Bool) return function (x : T) {
			return (!pred(x))?Some(x):None;
		}

		function testArgs<T>(i:Int, s:String, t:T) {
			return Std.string(t);
		}
		function h(i : Array<Int>) {
			return switch(i) {
				case [x]: 1;
				case isPair(_) => Some({ a : a, b : b }) if (a < 0): 42;
				case isPair(_) => Some({ a : is(even)(_) => Some(a), b : b }) : a+b;
				case isPair(_) => Some({ a : isNot(even)(_) => Some(a), b : b }) : a*b;
				case testArgs(1, "foo", _) => "[99,98,97]": 99;
				case var arr: 3;
			}
		}

		eq(3, h([]));
		eq(1, h([1]));
		eq(1, h([2]));
		eq(5, h([2, 3]));
		eq(3, h([1, 3]));
		eq(3, h([2, 3, 4]));
		eq(42, h([-1, 3]));
		eq(99, h([99,98,97]));
	}

	static function isPair<T>(t:Array<T>) return t.length == 2 ? Some({a:t[0], b:t[1]}) : None;

	static function even(i:Int) {
		return i & 1 == 0;
	}

	static function deref<T>(ref:MiniRef<T>) return ref.get();
}