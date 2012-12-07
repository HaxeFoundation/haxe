package unit;
import haxe.macro.Expr;

enum Tree<T> {
	Leaf(t:T);
	Node(l:Tree<T>, r:Tree<T>);
}

class TestMatch extends Test {
	@:macro static function getErrorMessage(e:Expr) {
		var result = try {
			haxe.macro.Context.typeof(e);
			"no error";
		} catch (e:Dynamic) Std.string(e.message);
		return macro $(result);
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
			case EIn(_, { expr : e, pos : p }) :
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
			case a in a.length == 3: "5:" + a.length;
			case []: "6";
			case a: "7";
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
			case EConst(CString(s)) in StringTools.startsWith(s, "foo"):
				"1";
			case EConst(CString(s)) in StringTools.startsWith(s, "bar"):
				"2";
			case EConst(CInt(i)) in switch(Std.parseInt(i) * 2) { case 4: true; case _: false; } :
				"3";
			case EConst(_):
				"4";
			case _:
				"5";
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
		
	function testNonExhaustiveness() {
		eq("This match is not exhaustive, these patterns are not matched: false", getErrorMessage(switch(true) {
			case true:
		}));
		eq("This match is not exhaustive, these patterns are not matched: OpNegBits | OpNeg", getErrorMessage(switch(OpIncrement) {
			case OpIncrement:
			case OpDecrement:
			case OpNot:
		}));
		eq("This match is not exhaustive, these patterns are not matched: Node(Leaf(_),_)", getErrorMessage(switch(Leaf("foo")) {
			case Node(Leaf("foo"), _):
			case Leaf(_):
		}));
		eq("This match is not exhaustive, these patterns are not matched: Leaf(_)", getErrorMessage(switch(Leaf("foo")) {
			case Node(_, _):
			case Leaf(_) in false:
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
	// all lines marked as // unused should give a warning
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
			case { s : a } :
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