private enum Tree<T> {
	Node(l:Tree<T>, r:Tree<T>);
	Leaf(v:T);
}

private enum Some {
    one(s1:String);
    pair(s1:String, s2:String);
    triad(s1:String, s2:String, s3:String);
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

	@:js("var a = new List();var _g_head = a.h;var _g_val = null;while(_g_head != null) {var tmp;_g_val = _g_head[0];_g_head = _g_head[1];tmp = _g_val;}")
	static function testListIteratorInline() {
		var a = new List();
		for (v in a) { }
	}

	@:js("var a = 1;var tmp;var v2 = a;tmp = a + v2;if(tmp > 0) {}")
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

	@:js("var a = [];var tmp;try {tmp = a[0];} catch( e ) {tmp = null;}if(tmp) {}")
	@:analyzer(no_check_has_effect)
	@:analyzer(no_local_dce)
	static function testInlineWithComplexExpr() {
		var a = [];
		if (_inlineWithComplexExpr(a, 0)) {}
	}

	inline static function _inlineWithComplexExpr(a, i) {
		return try a[i] catch (e:Dynamic) null;
	}

	@:js("var a = { v : [{ b : 1}]};a;var tmp;switch(a.v.length) {case 1:switch(a.v[0].b) {case 1:tmp = true;break;default:tmp = false;}break;default:tmp = false;}if(tmp) {}")
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

	@:js('var a = "";var tmp;var __ex0 = a;var _g = __ex0.toLowerCase();switch(_g) {case "e":tmp = 0;break;default:throw new Error();}var e = tmp;')
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

	@:js('false;')
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

	static function use<T>(t:T) { }
}
