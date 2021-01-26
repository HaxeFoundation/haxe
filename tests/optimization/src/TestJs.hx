import haxe.ds.List;

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

@:analyzer(no_user_var_fusion)
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

	@:js("var a = new haxe_ds_List();var _g_head = a.h;while(_g_head != null) _g_head = _g_head.next;")
	static function testListIteratorInline() {
		var a = new List();
		for (v in a) { }
	}

	@:js('var a = 1;var v2 = a;if(a + v2 > 0) {TestJs.use(a);}')
	@:analyzer(no_const_propagation)
	@:analyzer(no_copy_propagation)
	@:analyzer(no_local_dce)
	static function testInlineWithArgumentUsedMoreThanOnce() {
		var a = 1;
		if (_inlineWithArgumentUsedMoreThanOnce(a) > 0) {
			use(a);
		}
	}

	inline static function _inlineWithArgumentUsedMoreThanOnce(v) {
		var v2 = v;
		return v + v2;
	}

	@:js("var a = [];var tmp;try {tmp = a[0];} catch( _g ) {tmp = null;}tmp;")
	@:analyzer(no_local_dce)
	static function testInlineWithComplexExpr() {
		var a = [];
		if (_inlineWithComplexExpr(a, 0)) {}
	}

	inline static function _inlineWithComplexExpr(a:Array<Bool>, i:Int) {
		return try a[i] catch (e:Dynamic) null;
	}

	@:js("var a_v_0_b = 1;")
	@:analyzer(no_const_propagation, no_local_dce)
	static function testDeepMatchingWithoutClosures() {
		var a = {v: [{b: 1}]};
		a;
		if (switch (a) {
			case {v: [{b: 1}]}: true;
			default: false;
		}) {}
	}

	@:js('var a = [1,2,3];var _g = 0;while(_g < a.length) {var v = a[_g];++_g;TestJs.use(v + 2);}')
	static function testInlineFunctionWithAnonymousCallback() {
		var a = [1,2,3];
		inline function forEach(f) for (v in a) f(v);
		forEach(function(x) use(x + 2));
	}

	@:js('var a = "";var e;if(a.toLowerCase() == "e") {e = 0;} else {throw new Error();}')
	@:analyzer(no_const_propagation, no_local_dce, no_copy_propagation)
	static function testRValueSwitchWithExtractors() {
		var a = "";
		var e = switch (a) {
			case _.toLowerCase() => "e": 0;
			default: throw new js.lib.Error();
		}
	}

	@:js('TestJs.use("1" + "2" + "3" + "4");')
	static function testEnumValuePropagation1() {
		var n = Node(Node(Leaf("1"), Node(Leaf("2"), Leaf("3"))), Leaf("4"));
		switch (n) {
			case Node(Node(Leaf(s1), Node(Leaf(s2), Leaf(s3))), Leaf(s4)):
				use(s1 + s2 + s3 + s4);
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
		var object = { "hello" : "world"};
		TestJs.use(object);
	')
	static function testQuotedStructureFields1() {
		var object = {
			"hello": "world"
		}
		use(object);
	}

	@:js('
		var object = { "hello" : "world", world : "hello", "another" : "quote"};
		TestJs.use(object);
	')
	static function testQuotedStructureFields2() {
		var object = {
			'hello': "world",
			world: "hello",
			"another": "quote"
		}
		use(object);
	}

	@:js('
		var object = { "\'" : "world"};
		TestJs.use(object);
	')
	static function testQuotedStructureFields3() {
		var object = {
			"'": "world",
		}
		use(object);
	}

	@:analyzer(no_local_dce)
	@:js('var v0_x = 0;var v1_x = 1;var vRand_x = Math.random();')
	// #4558
	static function testInlineFunctionNew() {
		var v0 = new Inl(0);
		var v1 = new Inl(1);
		var vRand = new Inl(Math.random());
	}

	@:js("try {throw haxe_Exception.thrown(false);} catch( _g ) {}")
	static function testNoHaxeErrorUnwrappingWhenNotRequired() {
		try throw false catch (e:Dynamic) {}
	}

	@:js('try {throw haxe_Exception.thrown(false);} catch( _g ) {TestJs.use(haxe_Exception.caught(_g).unwrap());}')
	static function testHaxeErrorUnwrappingWhenUsed() {
		try throw false catch (e:Dynamic) use(e);
	}

	@:js('try {throw haxe_Exception.thrown(false);} catch( _g ) {if(typeof(haxe_Exception.caught(_g).unwrap()) != "boolean") {throw _g;}}')
	static function testHaxeErrorUnwrappingWhenTypeChecked() {
		try throw false catch (e:Bool) {};
	}

	@:js('try {throw haxe_Exception.thrown(false);} catch( _g ) {if(typeof(haxe_Exception.caught(_g).unwrap()) == "boolean") {TestJs.use(_g);} else {throw _g;}}')
	static function testGetOriginalException() {
		try throw false catch (e:Bool) use(js.Lib.getOriginalException());
	}

	@:js('try {throw haxe_Exception.thrown(false);} catch( _g ) {if(typeof(haxe_Exception.caught(_g).unwrap()) == "boolean") {throw _g;} else {throw _g;}}')
	static function testRethrow() {
		try throw false catch (e:Bool) js.Lib.rethrow();
	}

	@:js('TestJs.use(2);')
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
		TestJs.use(3);
	')
	static function testBinop() {
		var a = 1;
		var b = 2;
		use(a + b);
	}

	@:js('
		TestJs.use(false);
		TestJs.use(true);
		TestJs.use(true);
		TestJs.use(true);
		TestJs.use(false);
		TestJs.use(true);
		TestJs.use(true);
		TestJs.use(false);
		TestJs.use(false);
		TestJs.use(true);
		TestJs.use(false);
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
		var map_h = Object.create(null);
		map_h["some"] = 2;
		TestJs.use(2);
	')
	static function testIssue4731() {
		var map = new Map();
		var i = map["some"] = 2;
		use(i);
	}

	@:js('
		var x = TestJs.getInt();
		TestJs.getInt();
		TestJs.call([x,"foo"],TestJs.getInt());
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
		x = TestJs.getInt();
		TestJs.call(tmp,TestJs.getInt());
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
		TestJs.call(x++,TestJs.getInt());
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
		if(Math.random() < 0.5) {
			a = 2;
		}
		var b = "";
		if(Math.random() < 0.5) {
			b = "hello";
		}
		TestJs.use(a);
		TestJs.use(b);
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

	@:js('
		var a = TestJs.getInt();
		TestJs.use(a);
	')
	static function testCopyPropagation1() {
		var a = getInt();
		var b = a;
		use(b);
	}

	@:js('
		var a = TestJs.getInt();
		var b = a;
		a = TestJs.getInt();
		TestJs.use(b);
	')
	static function testCopyPropagation2() {
		var a = getInt();
		var b = a;
		a = getInt();
		use(b);
	}

	//@:js('
		//var b = TestJs.getInt();
		//TestJs.use(b);
	//')
	//static function testCopyPropagation3() {
		//var a;
		//{
			//var b = getInt();
			//a = b;
		//}
		//use(a);
	//}

	@:js('
		TestJs.getInt();
		if(TestJs.getInt() != 0) {
			throw haxe_Exception.thrown("meh");
		}
	')
	static function testIfInvert() {
		var tmp;
		var tmp2 = getInt();
		if (getInt() == 0) {
			tmp = tmp2;
		} else {
			throw "meh";
		}
	}

	@:js('
		var d1 = TestJs.intField;
		TestJs.call(TestJs.use(null),d1);
	')
	static function testInlineRebuilding1() {
		inlineCall(intField, use(null));
	}

	@:js('
		TestJs.call(TestJs.use(null),TestJs.stringField);
	')
	static function testInlineRebuilding2() {
		inlineCall(stringField, use(null));
	}

	@:js('
		var a = TestJs.getArray();
		var d1 = a[0]++;
		TestJs.call(a[1]++,d1);
	')
	static function testInlineRebuilding3() {
		var a = getArray();
		inlineCall(a[0]++, a[1]++);
	}

	@:js('
		var a = TestJs.getArray();
		a[0] = 1;
		a[1] = 2;
		TestJs.call(2,1);
	')
	static function testInlineRebuilding4() {
		var a = getArray();
		inlineCall(a[0] = 1, a[1] = 2);
	}

	@:js('
		var a = TestJs.getArray();
		var d1 = a[0] += 1;
		TestJs.call(a[1] += 1,d1);
	')
	static function testInlineRebuilding5() {
		var a = getArray();
		inlineCall(a[0] += 1, a[1] += 1);
	}

	@:js('
		var d1 = TestJs.call(1,2);
		TestJs.call(TestJs.call(3,4),d1);
	')
	static function testInlineRebuilding6() {
		inlineCall(call(1, 2), call(3, 4));
	}

	@:js('
		var d1 = TestJs.call(1,2);
		var d11 = TestJs.call(TestJs.call(3,4),d1);
		var d1 = TestJs.call(5,6);
		TestJs.call(TestJs.call(TestJs.call(7,8),d1),d11);
	')
	static function testInlineRebuilding7() {
		inlineCall(inlineCall(call(1, 2), call(3, 4)), inlineCall(call(5, 6), call(7, 8)));
	}

	@:js('
		var d1 = TestJs.call(1,2);
		var d11 = TestJs.call(TestJs.intField,d1);
		var d1 = TestJs.intField;
		TestJs.call(TestJs.call(TestJs.call(5,6),d1),d11);
	')
	static function testInlineRebuilding8() {
		inlineCall(inlineCall(call(1, 2), intField), inlineCall(intField, call(5, 6)));
	}

	@:js('
		var d1 = TestJs.call(TestJs.stringField,TestJs.call(1,2));
		TestJs.call(TestJs.call(TestJs.call(5,6),TestJs.stringField),d1);
	')
	static function testInlineRebuilding9() {
		inlineCall(inlineCall(call(1, 2), stringField), inlineCall(stringField, call(5, 6)));
	}

	@:js('
		var i = TestJs.getInt();
		var a = TestJs.getArray();
		a[i++] = i++;
	')
	static function testAssignmentSideEffect() {
		var i = getInt();
		var a = getArray();
		a[i++] = i++;
	}

	@:js('
		var i = 5;
		while(--i >= 0) TestJs.use(i);
	')
	static function testPrefixRebuilding() {
		var i:Int = 5;
		while (--i >= 0) use(i);
	}

	static inline function inlineCall<S, T>(d1:S, d2:T) {
		return call(d2, d1);
	}

	@:pure(false)
	static function getInt(?d:Dynamic) { return 1; }
	static function getArray() { return [0, 1]; }
	@:pure(false)
	static function call(d1:Dynamic, d2:Dynamic) { return d1; }
	@:pure(false)
	static public function use<T>(t:T) { return t; }

	static var intField = 12;
	static var stringField(default, never) = "foo";

	#if js_enums_as_arrays
	@:js('
		var _g = Type.typeof("");
		var v = _g[1] == 6 && _g[2] == String;
		TestJs.use(v);
	')
	#else
	@:js('
		var _g = Type.typeof("");
		var v = _g._hx_index == 6 && _g.c == String;
		TestJs.use(v);
	')
	#end
	static function testIssue4745() {
		var o = "";
		var v = Type.typeof(o).match(TClass(String));
		use(v);
	}

	@:js('
		var e = { };
		e["a"] = 30;
		TestJs.use(e);
	')
	@:analyzer(user_var_fusion)
	static function testIssue4948() {
		var e = new haxe.DynamicAccess();
		e["a"] = 30;
		use(e);
	}


	@:js('
		var tmp = "foo";
		Extern.test(tmp);
		var tmp = "bar";
		Extern.test(tmp);
		var closure = Extern.test;
		var tmp = "baz";
		closure(tmp);
	')
	static function testAsVar() {
		Extern.test("foo");
		var x = "bar";
		Extern.test(x);
		var closure = Extern.test;
		closure("baz");
	}

	static var v = false;

	@:js('')
	static function testIssue7874() {
		if (v && v) {}
	}

	@:js('')
	static function testIssue8751() {
		(2:Issue8751Int) * 3;
	}

	@:js('var v = "hi";TestJs.use(typeof(v) == "string" ? v : null);')
	static function testStdIsOptimizationSurvivesCast() {
		var value = "hi";
		use(as(value, String));
	}

	static inline function as<T>(v:Dynamic, c:Class<T>):Null<T> {
		return if (Std.isOfType(v, c)) v else null;
	}

	@:js('var f = function(x) {TestJs.use(x);};f(10);')
	static function testVarSelfAssignmentRemoved() {
		inline function g() return 0;
		function f(x:Int) {
			x = x + g();
			use(x);
		}

		f(10);
	}

	@:js('var f = function(x) {while(true) TestJs.use(x);};f(10);')
	static inline function testNoRedundantContinue() {
		inline function g() return true;
		function f(x:Int) {
			while (true) {
				TestJs.use(x);
				if (g()) continue;
			}
		}
		f(10);
	}

	@:js('
		var x = function() {return true;};
		var f = function(b) {
			if(x()) {b = true;}
			TestJs.use(b);
		};
		f(false);
	')
	static function testIssue9239_noDoubleNegation() {
		function x() return true;
		function f(b:Bool) {
			b = x() || b;
			TestJs.use(b);
		}
		f(false);
	}

	@:js('var a = !(!null);TestJs.use(a);')
	static function testIssue9239_dubleNegation_notOptimizedForNullBool() {
		inline function processNullBool(v:Null<Bool>):Bool {
			return !!v;
		}
		var a = processNullBool(null);
		TestJs.use(a);
	}

	@:js('
		var produce = function(producer) {return null;};
		produce(function(obj) {obj.id = 2;});
	')
	static function testIssue9181_arrowFunction_infersVoidReturn() {
		function produce<T>(producer: T -> Void): T {
			return null;
		}
		var result = produce((obj) -> {
			obj.id = 2;
		});
	}

	@:js('
		var voidFunc = function() {};
		TestJs.use(function() {voidFunc();});
		TestJs.use(function() {voidFunc();});
	')
	static function testIssue6420_voidFunction_noRedundantReturn() {
		function voidFunc() {}
		TestJs.use(function() return voidFunc());
		TestJs.use(() -> voidFunc());
	}

	@:js('
		var x = 1;
		var f = function(y) {
			return new Issue9227(x,y);
		};
		f(3);
	')
	static function testIssue9227_bind_lessClosures() {
		var f = Issue9227.new.bind(1);
		f(3);
	}
}

class Issue9227 {
	public function new(x:Int, y:Int) {}
}

extern class Extern {
	static public function test(e:haxe.extern.AsVar<String>):Void;
}

abstract Issue8751Int(Int) from Int {
	@:op(A * B) static public inline function add(a:Issue8751Int, b:Issue8751Int):Issue8751Int {
		return a.toInt() * b.toInt();
	}

	inline public function toInt():Int {
		return this;
	}
}