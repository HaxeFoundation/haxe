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
		var fn1:Void->Void;
		var fn2:Void->Void;
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

	@:analyzer(no_user_var_fusion)
	function testSyntaxNativeClassName() {
		eq("Array_hx", Syntax.nativeClassName(Array));
		eq("unit\\Annotation", Syntax.nativeClassName(Annotation));
		var cls = php.Web;
		eq("php\\Web", Syntax.nativeClassName(cls));
		var enm = Annotation;
		eq("unit\\Annotation", Syntax.nativeClassName(enm));
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

private typedef Func = Void->Void;

private abstract FunctionCaller(Func->Void) to Func->Void {
	public function new(f:Func->Void) this = f;
}

private abstract FunctionCallerWrapper( FunctionCaller ) from FunctionCaller to FunctionCaller {
	public inline function handle(callback:Func):Void
		return (this:Func->Void)(callback);
}