package unit;

typedef TestArray = Array<Test>;
private enum TestEnum { V1; V2; }

@:generic private class GenericTestClass<T> {}

class TestBasetypes extends Test {

	function testArray() {
		var a : Array<Null<Int>> = [1,2,3];
		eq( a.length, 3 );
		eq( a[0], 1 );
		eq( a[2], 3 );

		eq( a[3], null );
		eq( a[1000], null );
		eq( a[-1], null );

		a.remove(2);
		eq( a.length, 2);
		eq( a[0], 1 );
		eq( a[1], 3 );
		eq( a[2], null );

		var a : Array<Null<Int>> = [1,2,3];
		a.splice(1,1);
		eq( a.length, 2 );
		eq( a[0], 1 );
		eq( a[1], 3 );
		eq( a[2], null );
	}

	function testString() {
		eq( String.fromCharCode(77), "M" );
		unspec(function() String.fromCharCode(0));
		unspec(function() String.fromCharCode(-1));
		unspec(function() String.fromCharCode(256));
#if php
		eq( Std.string(null) + "x", "nullx" );
		eq( "x" + Std.string(null), "xnull" );
#else
		eq( null + "x", "nullx" );
		eq( "x" + null, "xnull" );
#end

		var abc = "abc".split("");
		eq( abc.length, 3 );
		eq( abc[0], "a" );
		eq( abc[1], "b" );
		eq( abc[2], "c" );

		var str = "abc";
		eq( str.charCodeAt(0), "a".code );
		eq( str.charCodeAt(1), "b".code );
		eq( str.charCodeAt(2), "c".code );
		eq( str.charCodeAt(-1), null );
		eq( str.charCodeAt(3), null );

		// substr tests
		var sentence:String = "Pack my box with five dozen liquor jugs.";
		eq(sentence.substr(0, 4), "Pack");
		eq(sentence.substr(5, 2), "my");
		eq(sentence.substr(0), sentence);
		eq(sentence.substr(35), "jugs.");
		eq(sentence.substr(40), "");
		eq(sentence.substr(42), "");
		eq(sentence.substr(-5, 4), "jugs");
		eq(sentence.substr(-5), "jugs.");
		eq(sentence.substr(-42), sentence);
		eq(sentence.substr(4, 0), "");
		eq(sentence.substr(0, -36), "Pack");

		// null should not be swallowed
		eq("hello" +null, "hellonull");
		eq(null + "hello", "nullhello");
		var x:Dynamic = null;
		//String const + Dynamic var with null ref
		eq("hello" +x, "hellonull");
		eq(x + "hello", "nullhello");
		var y:Dynamic = "hello";
		//Dynamic var + Dynamic var, where one is null, the other is a string:
		eq(x + y, "nullhello");
		eq(y + x, "hellonull");
		var x:String = null;
		//String const + String var with null ref
		eq("hello" +x, "hellonull");
		eq(x + "hello", "nullhello");

		var x = { hello:"world", val:5 };
		var xs = "" + x;
		// Output should contain hello followed by world, and val followed by 5.
		// The order of fields and operator between key and value remain unspecified.
		var h = xs.indexOf("hello");
		t(h != -1);
		t(xs.indexOf("world", h) != -1);
		h = xs.indexOf("val");
		t(h != -1);
		t(xs.indexOf("5", h) != -1);
		eq(x + "", xs);

		// Let's just make sure this is not 10 on any platform.
		eq(5 + "5", "55");
		eq("5" + 5, "55");
		eq("5" + 5.1, "55.1");

		// Some precedence checks.
		eq(1 + 1 + 1 + 1 + "1", "41");
		eq("1" + 1 + 1 + 1 + 1, "11111");
		eq(1 + 1 + "1" + 1 * 2, "212");

		// check recursive formating
		var x = [[1], [2, 3]];
		eq("" + x, "[[1],[2,3]]");

		// Brackets around array values should not be stripped.
		var x : Array<Dynamic> = [1, "hello"];
		eq("" + x, "[1,hello]");
		eq(x + "", "" + x);

		// This is also true for iterables that are arrays.
		var x:Iterable<Dynamic> = x;
		eq("" + x, "[1,hello]");
		eq(x + "", "" + x);

		// I don't think this should throw an exception on PHP.
		try {
			"" + x.iterator();
		} catch (e:Dynamic)	{
			Test.report("Could not convert Iterator to String");
		}

		var str = "he\nlo\"'";
		eq( Std.string(str), str);
		eq( Std.string([str]), "[" + str + "]");

		var e = MyEnum.C(0, "h");
		eq( Std.string(e), "C(0,h)");

		eq(Std.string([e]), "[C(0,h)]");

		var tester:String = "show me the (show me!) index of show me";
		eq(tester.lastIndexOf("show me"), 32);
		eq(tester.lastIndexOf("show me", 1), 0);
		eq(tester.lastIndexOf("show me",28), 13);
	}

	function testMath() {
		eq( Math.floor(-1.7), -2 );
		eq( Math.floor(-1.5), -2 );
		eq( Math.floor(-1.2), -2 );
		eq( Math.floor(1.7), 1 );
		eq( Math.floor(1.5), 1 );
		eq( Math.floor(1.2), 1 );
		eq( Math.ceil(-1.7), -1 );
		eq( Math.ceil(-1.5), -1 );
		eq( Math.ceil(-1.2), -1 );
		eq( Math.ceil(1.7), 2 );
		eq( Math.ceil(1.5), 2 );
		eq( Math.ceil(1.2), 2 );
		eq( Math.round(-1.7), -2 );
		eq( Math.round(-1.5), -1 );
		eq( Math.round(-1.2), -1 );
		eq( Math.round(1.7), 2 );
		eq( Math.round(1.5), 2 );
		eq( Math.round(1.2), 1 );


		// we don't specify Std.int and other to-int conversions when outside the Int range

		#if (js || flash)
		eq( Std.int( -10000000000.7), 0xABF41C00 );
		eq( Std.int( 10000000000.7), 0x540BE400 );

		eq( Std.int( -4294967296.7), 0 );
		eq( Std.int( -4294967296.001), 0 );
		eq( Std.int( 4294967296.7), 0 );
		eq( Std.int( 4294967296.001), 0 );
		eq( Std.int( -4294967295.7), 1 );
		eq( Std.int( -4294967295.001), 1 );
		eq( Std.int( 4294967295.7), -1 );
		eq( Std.int( 4294967295.001), -1 );

		eq( Std.int( -2147483648.7), 0x80000000 );
		eq( Std.int( -2147483648.001), 0x80000000 );
		eq( Std.int( 2147483648.7), 0x80000000 );
		eq( Std.int( 2147483648.001), 0x80000000 );
		eq( Std.int( -2147483647.7), 0x80000001 );
		eq( Std.int( -2147483647.001), 0x80000001 );
		eq( Std.int( 2147483647.7), 0x7FFFFFFF );
		eq( Std.int( 2147483647.001), 0x7FFFFFFF );


		#if (flash && !as3)
		eq( Math.floor( -10000000000.7), 0xABF41BFF);
		eq( Math.ceil( -10000000000.7), 0xABF41C00);
		eq( Math.round( -10000000000.7), 0xABF41BFF);
		#else
		eq( Math.floor( -10000000000.7)*1.0, -10000000001. );
		eq( Math.ceil( -10000000000.7)*1.0, -10000000000. );
		eq( Math.round( -10000000000.7)*1.0, -10000000001. );
		#end

		#end

		eq( Math.ffloor( -10000000000.7), -10000000001. );
		eq( Math.fceil( -10000000000.7), -10000000000. );
		eq( Math.fround( -10000000000.7), -10000000001. );
	}

	function testStringMap() {
		var h = new haxe.ds.StringMap<Null<Int>>();
		h.set("x", -1);
		h.set("abcd", 8546);
		eq( h.get("x"), -1);
		eq( h.get("abcd"), 8546 );
		eq( h.get("e"), null );

		var k = Lambda.array(h);
		k.sort(Reflect.compare);
		eq( k.join("#"), "-1#8546" );

		var k = Lambda.array( { iterator : h.keys } );
		k.sort(Reflect.compare);
		eq( k.join("#"), "abcd#x" );

		t( h.exists("x") );
		t( h.exists("abcd") );
		f( h.exists("e") );
		h.remove("abcd");
		t( h.exists("x") );
		f( h.exists("abcd") );
		f( h.exists("e") );
		eq( h.get("abcd"), null);

		h.set("x", null);
		t( h.exists("x") );
		t( h.remove("x") );
		f( h.remove("x") );
	}

	function testIntMap() {
		var h = new haxe.ds.IntMap<Null<Int>>();
		h.set(0, -1);
		h.set(-4815, 8546);
		eq( h.get(0), -1);
		eq( h.get(-4815), 8546 );
		eq( h.get(456), null );

		var k = Lambda.array(h);
		k.sort(Reflect.compare);
		eq( k.join("#"), "-1#8546" );

		var k = Lambda.array( { iterator : h.keys } );
		k.sort(Reflect.compare);
		eq( k.join("#"), "-4815#0" );

		t( h.exists(0) );
		t( h.exists(-4815) );
		f( h.exists(456) );
		h.remove(-4815);
		t( h.exists(0) );
		f( h.exists(-4815) );
		f( h.exists(456) );
		eq( h.get( -4815), null);

		h.set(65, null);
		t( h.exists(65) );
		t( h.remove(65) );
		f( h.remove(65) );

		var h = new haxe.ds.IntMap();
		h.set(1, ['a', 'b']);
		t( h.exists(1) );
		t( h.remove(1) );
		f( h.remove(1) );
	}

	function testMap() {
		var i = new Map();
		i[1] = 0;
		var x = 1;
		i[x++] += 4;
		eq(x, 2);
		eq(i[1], 4);
	}

	function testObjectKeyword() {
		// new is a keyword in Haxe
		var l = { "new": "test" };
		var prefix = #if as3 "_" #else "" #end;
		eq(Reflect.field(l, prefix + "new"), "test");
		// const is a keyword on some platforms but not in Haxe
		// check that with can still access it normally
		var o = { const : 6 }
		eq(o.const, 6);
		eq(Reflect.field(o, prefix+"const"), 6);
	}

	function testFormat() {
		eq('', "");
		eq('$', "$");
		eq('$$', "$");
		eq('x$*', "x$*");

		var x = 5, y = [];
		eq('$x', "5");
		eq('a$x$', "a5$");

		eq('${5}', "5");
		eq('${5}${2}', "52");
		eq('a${x}b', "a5b");
		eq('${x}${y}', "5[]");
	}

	function testAbstract() {
		var a = new MyAbstract(33);
		t( Std.is(a, Int) );
		eq( a.toInt(), 33 );
		var b = a;
		a.incr();
		eq( a.toInt(), 34 );
		eq( b.toInt(), 33 );
	}

	function testAbstractCast() {
		var s = "Abstract casting ::t::";
		// var from
		var tpl:unit.MyAbstract.TemplateWrap = s;
		t(Std.is(tpl, haxe.Template));
		t(Std.is(tpl.get(), haxe.Template));
		eq(tpl.get().execute( { t:"works!" } ), "Abstract casting works!");

		//var to
		var str:String = tpl;
		t(Std.is(str, String));
		eq(str, "Abstract casting really works!");

		// assign from
		var tpl:unit.MyAbstract.TemplateWrap;
		tpl = s;
		t(Std.is(tpl, haxe.Template));
		t(Std.is(tpl.get(), haxe.Template));
		eq(tpl.get().execute( { t:"works!" } ), "Abstract casting works!");

		//assign to
		var str:String;
		str = tpl;
		t(Std.is(str, String));
		eq(str, "Abstract casting really works!");

		// call arg from
		function from(tpl:unit.MyAbstract.TemplateWrap) {
			eq(tpl.get().execute( { t:"works!" } ), "Abstract casting works!");
		}
		from(s);

		// call arg to
		function from(s:String) {
			eq(s, "Abstract casting really works!");
		}
		from(tpl);

		// object decl from variant
		var obj: { tpl:unit.MyAbstract.TemplateWrap } = { tpl:s };
		eq(obj.tpl.get().execute( { t:"works!" } ), "Abstract casting works!");

		// object decl from
		var obj: { tpl:unit.MyAbstract.TemplateWrap };
		obj = { tpl:s };
		eq(obj.tpl.get().execute( { t:"works!" } ), "Abstract casting works!");

		// object decl to variant
		var obj: { s:String } = { s:tpl };
		eq(obj.s, "Abstract casting really works!");

		// object decl to
		var obj: { s:String };
		obj = { s:tpl };
		eq(obj.s, "Abstract casting really works!");

		// array from
		var arr:Array<unit.MyAbstract.TemplateWrap> = [s, "foo"];
		eq(arr[0].get().execute( { t:"works!" } ), "Abstract casting works!");
		eq(arr[1].get().execute( { } ), "foo");

		// array to
		var arr:Array<String> = [tpl];
		eq(arr[0], "Abstract casting really works!");

		// cast to return
		function returnAbstractCast():String {
			return new unit.MyAbstract.Meter(12.2);
		}

		eq(returnAbstractCast(), "12.2m");

		// switch
		function switchMe(b):String {
			return switch(b) {
				case true: new unit.MyAbstract.Meter(12.2);
				default: new unit.MyAbstract.Meter(2.4);
			}
		}

		eq(switchMe(true), "12.2m");
		eq(switchMe(false), "2.4m");

		// ctor
		var m:unit.MyAbstract.Meter = 3000;
		var c = new unit.MyAbstract.MyClassWithAbstractArgCtor(m);
		feq(c.km, 3);
	}

	function testAbstractToAbstractCast() {
		var m:unit.MyAbstract.Meter = 122.2;
		var km:unit.MyAbstract.Kilometer = m;
		feq(km, 0.1222);
	}

	function testAbstractTypeParameters() {
		var hash1:unit.MyAbstract.MyHash<String> = ["k1", "v1", "k2", "v2"];
		eq("v1", hash1.get("k1"));
		eq("v2", hash1.get("k2"));
		var hash1:unit.MyAbstract.MyHash<Int> = [1, 2, 3, 4];
		eq(2, hash1.get("_s1"));
		eq(4, hash1.get("_s3"));
	}

	function testAbstractToString() {
		var km:unit.MyAbstract.Kilometer = 12.5;
		var m:unit.MyAbstract.Meter = 12.5;
		eq("12.5km", km);
		eq("12.5m", m);
		eq("Distance: 12.5km", "Distance: " + km);
		eq("Distance: 12.5m", "Distance: " + m);
	}

	function testAbstractInline() {
		eq(getAbstractValue(1), 2);
		eq(unit.MyAbstract.MyAbstractCounter.counter, 1);
		eq(getAbstractValue(2), 3);
		eq(unit.MyAbstract.MyAbstractCounter.counter, 2);
		eq(getAbstractValue(3), 4);
		eq(unit.MyAbstract.MyAbstractCounter.counter, 3);
	}

	inline function getAbstractValue(a:unit.MyAbstract.MyAbstractCounter) {
		return a.getValue();
	}

	function testAbstractOperatorOverload() {
		var v1:unit.MyAbstract.MyVector = new unit.MyAbstract.MyPoint3(1, 1, 1);
		var v2:unit.MyAbstract.MyVector = new unit.MyAbstract.MyPoint3(1, 2, 3);
		eq("(2,3,4)", v1 + v2);
#if !lua
		eq("(2,4,6)", v2 * 2.);
#end
		var v1Old = v1;
		v1 *= 2.;
#if !lua
		eq("(2,2,2)", v1);
#end
		eq(v1Old, v1);
		var v3 = v1 * 2.;
#if !lua
		eq("(4,4,4)", v3);
#end
		f(v1 == v3);

		var i:unit.MyAbstract.MyInt = 1;
		eq(2, i + i);
		i = i + i;
		eq(2, i);

		var s = "";
		function getString() {
			s += "b";
			return s;
		}

		var r:unit.MyAbstract.MyInt = 5;
		eq("aaaaa", r * "a");
		eq("aaaaa", "a" * r);
		eq("bbbbb", r * getString());
		eq("bbbbbbbbbb", getString() * r);

		var v:unit.MyAbstract.MyInt = 5;
		eq("abcde", "abcdefghijk" / v);
	}

	function testAbstractSetter() {
		var as = new unit.MyAbstract.MyAbstractSetter();
		as.value = "foo";
		eq(as.value, "foo");
	}

	function testAbstractMemberCall() {
		var as = new MyAbstract.MyAbstractThatCallsAMember(2);
		eq(3, as);
	}

	function testAbstractMultitypeInline() {
		var a = new unit.MyAbstract.MySpecialString("My debugging abstract");
		eq("debugging abstract", a.substr(3));
	}

	@:analyzer(no_local_dce)
	function testOptionalStructureFields() {
		var a:{?f:Int} = {};
		eq(a.f, null);

		var o:Dynamic = {};
		var a:{?f:Int} = o;
		eq(a.f, null);

		var i:Dynamic = 1;
		unspec(function() {
			var a:{?f:Int} = i; // allowed to fail at runtime
			unspec(function() a.f);
		});
	}

	function testComplexTypeLiteral() {
		var arrayOfTest = :Array<Test>,
			testArrayLiteral = :TestArray,
			qualifiedLiteral = :unit.TestBasetypes.TestArray,
			testArrayIdentifier = TestArray,
			generic1 = :GenericTestClass<Int>,
			generic2 = :GenericTestClass<Float>,
			enumLiteral = :TestEnum;
		eq(arrayOfTest, arrayOfTest);
		eq(arrayOfTest, testArrayLiteral);
		eq(arrayOfTest, qualifiedLiteral);
		eq(arrayOfTest, testArrayIdentifier);
		t((arrayOfTest:Class<Dynamic>) != (generic1:Class<Dynamic>));
		t((generic1:Class<Dynamic>) != (generic2:Class<Dynamic>));
		eq(enumLiteral, enumLiteral);
	}
}
