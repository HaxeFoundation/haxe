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
	static function testInlineWithArgumentUsedMoreThanOnce() {
		var a = 1;
		if (_inlineWithArgumentUsedMoreThanOnce(a) > 0) { }
	}

	inline static function _inlineWithArgumentUsedMoreThanOnce(v) {
		var v2 = v;
		return v + v2;
	}

	@:js("var a = [];var tmp;try {tmp = a[0];} catch( e ) {tmp = null;}if(tmp) {}")
	static function testInlineWithComplexExpr() {
		var a = [];
		if (_inlineWithComplexExpr(a, 0)) {}
	}

	inline static function _inlineWithComplexExpr(a, i) {
		return try a[i] catch (e:Dynamic) null;
	}

	@:js("var a = { v : [{ b : 1}]};a;var tmp;switch(a.v.length) {case 1:switch(a.v[0].b) {case 1:tmp = true;break;default:tmp = false;}break;default:tmp = false;}if(tmp) {}")
	@:analyzer(no_const_propagation)
	static function testDeepMatchingWithoutClosures() {
		var a = {v: [{b: 1}]};
		a;
		if (switch (a) {
			case {v: [{b: 1}]}: true;
			default: false;
		}) {}
	}
}
