private enum Tree<T> {
	Node(l:Tree<T>, r:Tree<T>);
	Leaf(v:T);
}

private enum Some {
    one(s1:String);
    pair(s1:String, s2:String);
    triad(s1:String, s2:String, s3:String);
}

class Inl{
	public var x:Float;
	public inline function new(f){
		x = f;
	}
}

private enum EnumFlagTest {
	EA;
	EB;
	EC;
}

class TestJs {
	//@:js('var x = 10;"" + x;var x1 = 10;"" + x1;var x2 = 10.0;"" + x2;var x3 = "10";x3;var x4 = true;"" + x4;')
	//static function testStdString() {
	//var x = 10;
	//Std.string(x);
	//var x:UInt = 10;
	//Std.string(x);
	//var x = 10.0;
	//Std.string(x);
	//var x = "10";
	//Std.string(x);
	//var x = true;
	//Std.string(x);
	//}

	@:js("var a = new List();var _g_head = a.h;while(_g_head != null) _g_head = _g_head.next;")
	static function testListIteratorInline() {
		var a = new List();
		for (v in a) { }
	}

	@:js("var a = 1;var tmp;var v2 = a;tmp = a + v2;tmp;")
	@:analyzer(no_const_propagation)
	@:analyzer(no_check_has_effect)
	@:analyzer(no_local_dce)
	static function testInlineWithArgumentUsedMoreThanOnce() {
		var a = 1;
		if (_inlineWithArgumentUsedMoreThanOnce(a) > 0) { }
	}

	inline static function _inlineWithArgumentUsedMoreThanOnce(v) {
		var v2 = v;
		return v + v2;
	}

	@:js("var a = [];a;")
	@:analyzer(no_check_has_effect)
	@:analyzer(no_local_dce)
	static function testInlineWithComplexExpr() {
		var a = [];
		if (_inlineWithComplexExpr(a, 0)) {}
	}

	inline static function _inlineWithComplexExpr(a, i) {
		return try a[i] catch (e:Dynamic) null;
	}

	@:js("var a = { v : [{ b : 1}]};a;var tmp;switch(a.v.length) {case 1:switch(a.v[0].b) {case 1:tmp = true;break;default:tmp = false;}break;default:tmp = false;}tmp;")
	@:analyzer(no_const_propagation, no_local_dce, no_check_has_effect)
	static function testDeepMatchingWithoutClosures() {
		var a = {v: [{b: 1}]};
		a;
		if (switch (a) {
			case {v: [{b: 1}]}: true;
			default: false;
		}) {}
	}

	@:js("var a = [1,2,3];var _g = 0;while(_g < a.length) {var v = a[_g];++_g;console.log(v + 2);}")
	static function testInlineFunctionWithAnonymousCallback() {
		var a = [1,2,3];
		inline function forEach(f) for (v in a) f(v);
		forEach(function(x) trace(x + 2));
	}

	@:js('var a = "";var e;var __ex0 = a;var _g = __ex0.toLowerCase();switch(_g) {case "e":e = 0;break;default:throw new Error();}')
	@:analyzer(no_const_propagation, no_local_dce)
	static function testRValueSwitchWithExtractors() {
		var a = "";
		var e = switch (a) {
			case _.toLowerCase() => "e": 0;
			default: throw new js.Error();
		}
	}

	@:js('console.log("1" + "2" + "3" + "4");')
	static function testEnumValuePropagation1() {
		var n = Node(Node(Leaf("1"), Node(Leaf("2"), Leaf("3"))), Leaf("4"));
		switch (n) {
			case Node(Node(Leaf(s1), Node(Leaf(s2), Leaf(s3))), Leaf(s4)):
				trace(s1 + s2 + s3 + s4);
			case _:
		}
	}

	@:js('')
	static function testEnumValuePropagation2() {
		var v = pair("foo", "bar");
		var x = switch (v) {
			case one(s1): verify(s1);
			case pair(s1, s2): verify(s1) && verify(s2);
			case triad(s1, s2, s3): verify(s1) && verify(s2) && verify(s3);
		}
	}

	static inline function verify(s1) return s1 == "foo";

	@:js('
		var object = { \'hello\' : "world"};
		TestJs["use"](object);
	')
	static function testQuotedStructureFields1() {
		var object = {
			"hello": "world"
		}
		use(object);
	}

	@:js('
		var object = { \'hello\' : "world", world : "hello", \'another\' : "quote"};
		TestJs["use"](object);
	')
	static function testQuotedStructureFields2() {
		var object = {
			'hello': "world",
			world: "hello",
			"another": "quote"
		}
		use(object);
	}

	@:analyzer(no_local_dce)
	@:js('var v0_x = 0;var v1_x = 1;var f = Math.random();var vRand_x = f;')
	// #4558
	static function testInlineFunctionNew() {
		var v0 = new Inl(0);
		var v1 = new Inl(1);
		var vRand = new Inl(Math.random());
	}

	@:js("try {throw new js__$Boot_HaxeError(false);} catch( e ) {}")
	static function testNoHaxeErrorUnwrappingWhenNotRequired() {
		try throw false catch (e:Dynamic) {}
	}

	@:js("try {throw new js__$Boot_HaxeError(false);} catch( e ) {if (e instanceof js__$Boot_HaxeError) e = e.val;console.log(e);}")
	static function testHaxeErrorUnwrappingWhenUsed() {
		try throw false catch (e:Dynamic) trace(e);
	}

	@:js("try {throw new js__$Boot_HaxeError(false);} catch( e ) {if (e instanceof js__$Boot_HaxeError) e = e.val;if( js_Boot.__instanceof(e,Bool) ) {} else throw(e);}")
	static function testHaxeErrorUnwrappingWhenTypeChecked() {
		try throw false catch (e:Bool) {};
	}


	@:js('TestJs["use"](2);')
	static function testIssue3938() {
		var a = 1;
		if (a == 1) {
			a = 2;
		} else {
			a = 3;
		}
		use(a);
	}

	@:js('
		TestJs["use"](3);
	')
	static function testBinop() {
		var a = 1;
		var b = 2;
		use(a + b);
	}

	@:js('
		TestJs["use"](false);
		TestJs["use"](true);
		TestJs["use"](true);
		TestJs["use"](true);
		TestJs["use"](false);
		TestJs["use"](true);
		TestJs["use"](true);
		TestJs["use"](false);
		TestJs["use"](false);
		TestJs["use"](true);
		TestJs["use"](false);
	')
	static function testEnumValueFlags() {
		var flags = new haxe.EnumFlags();
		use(flags.has(EA));
		flags = new haxe.EnumFlags(1);
		use(flags.has(EA));

		// set
		flags.set(EB);
		use(flags.has(EA));
		use(flags.has(EB));
		use(flags.has(EC));

		// unset
		flags.unset(EC);
		use(flags.has(EA));
		use(flags.has(EB));
		use(flags.has(EC));
		flags.unset(EA);
		use(flags.has(EA));
		use(flags.has(EB));
		use(flags.has(EC));
	}

	@:js('
		var map = new haxe_ds_StringMap();
		var tmp;
		if(__map_reserved.some != null) map.setReserved("some",2); else map.h["some"] = 2;
		tmp = 2;
		var i = tmp;
		TestJs["use"](i);
	')
	static function testIssue4731() {
        var map = new Map();
        var i = map["some"] = 2;
		use(i);
		// This is not const-propagated because StringMap introduced unbound variables
	}

	@:js('
		var x = TestJs.getInt();
		var tmp;
		TestJs.getInt();
		tmp = TestJs.getInt();
		TestJs.call([x,"foo"],tmp);
	')
	static function testMightBeAffected1() {
		var x = getInt();
		call([x, "foo"], {
			getInt();
			getInt();
		});
	}

	@:js('
		var x = TestJs.getInt();
		var tmp = [x,"foo"];
		var tmp1;
		x = TestJs.getInt();
		tmp1 = TestJs.getInt();
		TestJs.call(tmp,tmp1);
	')
	static function testMightBeAffected2() {
		var x = getInt();
		call([x, "foo"], {
			x = getInt();
			getInt();
		});
	}

	@:js('
		var x = TestJs.getInt();
		var tmp = x;
		var tmp1;
		++x;
		tmp1 = TestJs.getInt();
		TestJs.call(tmp,tmp1);
	')
	static function testMightBeAffected3() {
		var x = getInt();
		call(x, {
			x++;
			getInt();
		});
	}

	@:js('
		var a = 0;
		if(Math.random() < 0.5) a = 2;
		var b = "";
		if(Math.random() < 0.5) b = "hello";
		TestJs["use"](a);
		TestJs["use"](b);
	')
	static function testIssue4739() {
		var a = 0;
		if (Math.random() < 0.5)
			a += 2;

		var b = "";
		if (Math.random() < 0.5)
			b = b + "hello";

		use(a);
		use(b);
	}

	static function getInt(?d:Dynamic) { return 1; }
	static function call(d1:Dynamic, d2:Dynamic) { return d1; }
	static function use<T>(t:T) { }
}