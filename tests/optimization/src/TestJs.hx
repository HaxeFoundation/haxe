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

	@:js("var a = new List();var _g_head = a.h;var _g_val = null;while(_g_head != null) {var tmp;_g_val = _g_head[0];_g_head = _g_head[1];tmp = _g_val;tmp;}")
	static function testListIteratorInline() {
		var a = new List();
		for (v in a) { }
	}

	@:js("var a = 1;var tmp;var v2 = a;tmp = a + v2;if(tmp > 0) {}")
	@:analyzer(no_const_propagation)
	@:analyzer(no_check_has_effect)
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

	@:js('var a = "";var tmp;var __ex0 = a;var _g = __ex0.toLowerCase();switch(_g) {case "e":tmp = 0;break;default:throw false;}var e = tmp;')
	@:analyzer(no_const_propagation, no_local_dce)
	static function testRValueSwitchWithExtractors() {
		var a = "";
		var e = switch (a) {
			case _.toLowerCase() => "e": 0;
			default: throw false;
		}
	}
}
