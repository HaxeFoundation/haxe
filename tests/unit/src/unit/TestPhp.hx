package unit;

import php.*;

using StringTools;

class TestPhp extends Test
{
	var list : Array<Dynamic>;
	var list2 : Array<Dynamic>;
	function empty() return true;
	function testAbstractEnum() {
		eq(Abstract.getName(), "Abstract");
		var x = Const("foo");
		var s = switch(x) {
			case Const(s): s;
			case Abstract: "null";
		}
		eq("foo", s);
	}

	function testAbstractKeywordAsFunction() {
		t(empty());
	}

	function testList2() {
		list2 = new Array<Dynamic>();
		for( l in list2 ) {} // fails here
		t(true);
	}

	function testList() {
		list = new Array<Dynamic>();
		for( l in list ) {} // fails here
		t(true);
	}

	var switchVal = "1";
	function testIssue7257_looseSwitchComparison() {
		var result = switch (switchVal) {
			case "01": false;
			case "1": true;
			default: false;
		}
		t(result);
	}

	function testCustomArrayDecl() {
		var a = Syntax.customArrayDecl([1 => 'hello', 'world' => true]);
		var keys:Array<Dynamic> = [];
		var values:Array<Dynamic> = [];
		for(k => v in a) {
			keys.push(k);
			values.push(v);
		}
		aeq(([1, 'world']:Array<Dynamic>), keys);
		aeq((['hello', true]:Array<Dynamic>), values);
	}

	function testIssue1828() {
		var x = try {
			throw "foo";
		} catch (e:String) {
			true;
		}
		t(x);
	}

	function testIssue1521() {
		var pattern = "$a$b";
		var result = pattern.replace("$a","A").replace("$b","B");
		eq("AB", result);
	}

	function testIssue2146() {
		f(Class2146.test());
	}

	function testIssue6698() {
		var arr = new NativeAssocArray<Dynamic>();
		var innerArr = new NativeAssocArray<Int>();
		innerArr['one'] = 1;
		innerArr['two'] = 2;
		arr['inner'] = innerArr;

		var obj = Lib.objectOfAssociativeArray(arr);
		var innerObj = obj.inner;
		t(Global.is_object(innerObj));
		eq(1, innerObj.one);
		eq(2, innerObj.two);
	}

	function testStupidShit9000() {
		var f = make();
		f.handle( function() {
			eq("ok", "ok");
		});
	}

	@:analyzer(ignore)
	function testIssue9924() {
		var v = Std.random(10);

		var GLOBALS = v;
		var _SERVER = v;
		var _GET = v;
		var _POST = v;
		var _FILES = v;
		var _COOKIE = v;
		var _REQUEST = v;
		var _ENV = v;
		// var _SESSION = v; //not defined in CLI

		utest.Assert.notEquals(GLOBALS, SuperGlobal.GLOBALS);
		utest.Assert.notEquals(_SERVER, SuperGlobal._SERVER);
		utest.Assert.notEquals(_GET, SuperGlobal._GET);
		utest.Assert.notEquals(_POST, SuperGlobal._POST);
		utest.Assert.notEquals(_FILES, SuperGlobal._FILES);
		utest.Assert.notEquals(_COOKIE, SuperGlobal._COOKIE);
		utest.Assert.notEquals(_REQUEST, SuperGlobal._REQUEST);
		utest.Assert.notEquals(_ENV, SuperGlobal._ENV);
		// utest.Assert.notEquals(_SESSION, SuperGlobal._SESSION);
	}

	inline static function make():FunctionCallerWrapper {
		return new FunctionCaller(function(f) f());
	}

	/**
		Check compiler will generate proper function signature with `Ref<T>` arguments
	**/
	function testRef() {
		function modify(i:Ref<Int>) i = 10; //avoid inlining

		var i = 0;
		modify(i);
		eq(i, 10);

		var d = new DummyForRef();
		modify(d.getThis().field);
		eq(d.field, 10);
	}

	/**
		Check compiler will not fuse `Ref<T>` vars
	**/
	// function testRefAsVar() {
	// 	var modify = function(i:Ref<Int>) i = 10;
	// 	var i = 0;
	// 	modify(i);
	// 	t(true);
	// }

	function testAddOrConcat() {
		var result = add(1, 'a');
		eq(result, '1a');

		var result = add('a', 1);
		eq(result, 'a1');

		var result = add('a', 'b');
		eq(result, 'ab');

		var result = add(1, null);
		eq(result, 1);

		var result = add(null, 1);
		eq(result, 1);

		var result = add('a', null);
		eq(result, 'anull');

		var result = add(null, 'b');
		eq(result, 'nullb');
	}
	function add(a:Dynamic, b:Dynamic):Dynamic return a + b;

	function testStringClosureType() {
		var fn:Dynamic = 'foo'.toUpperCase;
		var className = Type.getClassName(Type.getClass(fn)).split('.').pop();
		eq('HxDynamicStr', className);
		eq('FOO', fn());
	}

	function testStringAsAnon() {
		var str = 'bar';
		var anon:{
			function toUpperCase() : String;
			function toLowerCase() : String;
			function charAt( index : Int) : String;
			function indexOf( str : String, ?startIndex : Int ) : Int;
			function lastIndexOf( str : String, ?startIndex : Int ) : Int;
			function split( delimiter : String ) : Array<String>;
			function toString() : String;
			function substring( startIndex : Int, ?endIndex : Int ) : String;
			function charCodeAt( index : Int) : Null<Int> ;
			function substr( pos : Int, ?len : Int ) : String ;
		} = str;

		eq(str.charAt(0), anon.charAt(0));
		eq(str.charCodeAt(0), anon.charCodeAt(0));
		eq(str.indexOf("a"), anon.indexOf("a"));
		eq(str.lastIndexOf("r"), anon.lastIndexOf("r"));
		eq(str.split("a")[1], anon.split("a")[1]);
		eq(str.substr(1),anon.substr(1, 2));
		eq(str.substring(0, 2), anon.substring(0, 2));
		eq(str, anon.toLowerCase());
		eq(str.toUpperCase(), anon.toUpperCase());
		eq(str.toString(), anon.toString());
	}

	function testClosureComparison() {
		var fn1:()->Void;
		var fn2:()->Void;
		eq(ClosureDummy.testStatic, ClosureDummy.testStatic);
		//Waiting for a fix: https://github.com/HaxeFoundation/haxe/issues/6719
		// t(ClosureDummy.testStatic == ClosureDummy.testStatic);
		t((ClosureDummy:Dynamic).testStatic == (ClosureDummy:Dynamic).testStatic);
		fn1 = (ClosureDummy:Dynamic).testStatic;
		fn2 = (ClosureDummy:Dynamic).testStatic;
		t(fn1 == fn2);

		var inst = new ClosureDummy();
		eq(inst.test, inst.test);
		//Waiting for a fix: https://github.com/HaxeFoundation/haxe/issues/6719
		// t(inst.test == inst.test);
		t((inst:Dynamic).test == (inst:Dynamic).test);
		fn1 = (inst:Dynamic).test;
		fn2 = (inst:Dynamic).test;
		t(fn1 == fn2);
		var a = [fn1];
		t(a.indexOf(fn2) == 0);
		t(a.remove(fn2));

		var inst2 = new ClosureDummy();
		//Waiting for a fix: https://github.com/HaxeFoundation/haxe/issues/6719
		// f(inst.test == inst2.test);
		a = [fn1];
		fn2 = (inst2:Dynamic).test;
		t(a.indexOf(fn2) < 0);
		f(a.remove(fn2));
	}

	function testSyntaxInstanceof() {
		var o = new ClosureDummy();
		var phpClassName = Boot.castClass(ClosureDummy).phpClassName;
		t(Syntax.instanceof(o, ClosureDummy));
		t(Syntax.instanceof(o, phpClassName));
	}

	var a = 2;
	function testSyntaxCodeParens() {
		eq(8, Syntax.code("{0} * {1}", a, a + a));
	}

	@:analyzer(no_user_var_fusion)
	function testSyntaxNativeClassName() {
		var phpPrefix = #if php_prefix Boot.getPrefix() + "\\" #else "" #end;
		eq(phpPrefix + "Array_hx", Syntax.nativeClassName(Array));
		eq(phpPrefix + "unit\\Annotation", Syntax.nativeClassName(Annotation));
		var cls = php.Web;
		eq(phpPrefix + "php\\Web", Syntax.nativeClassName(cls));
		var enm = Annotation;
		eq(phpPrefix + "unit\\Annotation", Syntax.nativeClassName(enm));
	}

	function testNativeString() {
		var expected:php.NativeString = '123456';

		var actual = '';
		for(c in expected) {
			actual += c;
		}
		eq(expected, actual);

		var actual = '';
		var keys = [];
		for(i => c in expected) {
			keys.push(i);
			actual += c;
		}
		eq(expected, actual);
		aeq([0, 1, 2, 3, 4, 5], keys);
	}

	function testNativeArray() {
		var keys:Array<Dynamic> = ['hello', 12];
		var values:Array<Dynamic> = [10, 'world'];
		var a = new php.NativeArray();
		for(i in 0...keys.length) {
			a[keys[i]] = values[i];
		}

		var actualValues = [for(v in a) v];
		aeq(values, actualValues);

		var actualKeys:Array<Dynamic> = [];
		var actualValues:Array<Dynamic> = [];
		for(k => v in a) {
			actualKeys.push(k);
			actualValues.push(v);
		}
		aeq(keys, actualKeys);
		aeq(values, actualValues);
	}

	function testNativeIndexedArray() {
		var expected = [10, 20, 30];
		var a = new NativeIndexedArray<Int>();
		for(v in expected) {
			a.push(v);
		}

		var actual = [for(v in a) v];
		aeq(expected, actual);

		var indexes = [];
		var values = [];
		for(i => v in a) {
			indexes.push(i);
			values.push(v);
		}
		aeq([for(i in 0...expected.length) i], indexes);
		aeq(expected, values);

		eq('[10,20,30]', Std.string(a));
	}

	function testNativeAssocArray() {
		var keys = ['one', 'two'];
		var values = [1, 2];
		var a = new NativeAssocArray<Int>();
		for(i in 0...keys.length) {
			a[keys[i]] = values[i];
		}

		var actualValues = [for(v in a) v];
		aeq(values, actualValues);

		var actualKeys = [];
		var actualValues = [];
		for(k => v in a) {
			actualKeys.push(k);
			actualValues.push(v);
		}
		aeq(keys, actualKeys);
		aeq(values, actualValues);
	}
}

private class ClosureDummy {
	static public function testStatic() {}
	public function new() {}
	public function test() {}
}

private class DummyForRef {
	public var field:Int = 0;
	public function new() {}
	public function getThis() return this;
}

class Class2146 {
	var array:Array<Class2146>;
	function new() {
		array = new Array<Class2146>();
	}

	public static function test() {
		var a = new Class2146();
		var b = new Class2146();
		var c = new Class2146();
		a.array.push(b);
		b.array.push(a);
		c.array.push(a);
		return Lambda.has(c.array,b);
	}
}

enum Annotation {
	Abstract;
	Const(i:String);
}

private typedef Func = ()->Void;

private abstract FunctionCaller(Func->Void) to Func->Void {
	public function new(f:Func->Void) this = f;
}

private abstract FunctionCallerWrapper( FunctionCaller ) from FunctionCaller to FunctionCaller {
	public inline function handle(callback:Func):Void
		return (this:Func->Void)(callback);
}