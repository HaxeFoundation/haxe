package unit;
import haxe.macro.Expr;

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

class TestMatch extends Test {
	static macro function getErrorMessage(e:Expr) {
		var result = try {
			haxe.macro.Context.typeof(e);
			"no error";
		} catch (e:Dynamic) Std.string(e.message);
		return macro $v{result};
	}
	
	static function switchNormal(e:Expr):String {
		return switch(e.expr) {
			case EConst(CString(s)): s;
			case EParenthesis( { expr : EConst(CString(s)) } )
			| EUntyped( { expr : EConst(CString(s)) } ):
				s;
			case EField(_, s):
				s;
			case EArray(_, { expr : EConst(CInt(i) | CFloat(i)) } ):
				Std.string(i);
			case EIn(_, { expr : e, pos : _ }) :
				Std.string(e);
			case _:
				"not_found";
		}
	}
	
	static function switchCapture(e:Expr) {
		return switch(e) {
			case { expr : EConst(const = (CString("foobar") | CInt("9"))) } :
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
			case a if (a.length == 3): "5:" + a.length;
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
			case EConst(CString(s)) if (StringTools.startsWith(s, "foo")):
				"1";
			case EConst(CString(s)) if (StringTools.startsWith(s, "bar")):
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
			case haxe.Template: "haxe.Template";
			case a: "other: " +Type.getClassName(a);
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
		eq("[EConst(CInt(22)),EConst(CString(foo))]", switchArray(macro [22,"foo"]));
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
		
		eq("EConst(CString(foobar)):12", switchCrazy(macro untyped ("foobar"[12])));
		
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
			case x: '_:$x';
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
		eq("haxe.Template", switchClass(haxe.Template));
		eq("other: unit.TestMatch", switchClass(TestMatch));
	}
		
	function testNonExhaustiveness() {
		eq("Unmatched patterns: false", getErrorMessage(switch(true) {
			case true:
		}));
		eq("Unmatched patterns: OpNegBits | OpNeg", getErrorMessage(switch(OpIncrement) {
			case OpIncrement:
			case OpDecrement:
			case OpNot:
		}));
		eq("Unmatched patterns: Node(Leaf(_),_)", getErrorMessage(switch(Leaf("foo")) {
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
		eq("Unmatched patterns: [_,false,_]", getErrorMessage(switch [1, true, "foo"] {
			case [_, true, _]:
		}));
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
		eq("Variable l must appear exactly once in each sub-pattern", getErrorMessage(switch(Leaf("foo")) {
			case Node(l = Leaf(l), _):
		}));
		eq("String should be unit.Tree<String>", getErrorMessage(switch(Leaf("foo")) {
			case Node(l = Leaf(_), _) | Leaf(l):
		}));
	}
	
	#if false
	 //all lines marked as // unused should give an error
	function testRedundance() {
		switch(true) {
			case false:
			case true:
			case false: // unused
		}
		
		switch(true) {
			case false | true:
			case true: // unused
			case false: // unused
		}
		
		switch(true) {
			case false
			| false: // unused
			case true:
		}
		
		switch(Leaf(true)) {
			case Leaf(true):
			case Leaf(false):
			case Leaf(x): // unused
			case Node(_):
		}
		
		switch({s:"foo"}) {
			case { s : "foo" } :
			case { s : a } : // Warning : This variable is unused
			case _: // unused
		}
		
		switch( { s:"foo", t:"bar" } ) {
			case { s : "foo" }:
			case { t : "bar" }:
			case { s : "foo", t:"bar" }: // unused
			case _:
		}
	}
	#end
}