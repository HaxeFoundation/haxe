package unit;

class TestJson extends Test {

	function testNativeJson() {
		var str = haxe.Json.stringify( { x : -4500, y : 1.456, a : ["hello", "wor'\"\n\t\rd"] } );
		str = str.substr(1, str.length - 2); // remove {}
		var parts = str.split(",");
		t( parts.remove('"x":-4500') );
		t( parts.remove('"y":1.456') );
		t( parts.remove('"a":["hello"') );
		t( parts.remove('"wor\'\\"\\n\\t\\rd"]') );
		eq( parts.join("#"), "" );

		function id(v:Dynamic,?pos:haxe.PosInfos) eq(haxe.Json.parse(haxe.Json.stringify(v)),v, pos);
		function deepId(v:Dynamic) {
			var str = haxe.Json.stringify(v);
			eq(haxe.Json.stringify(haxe.Json.parse(str)), str);
		}

		id(true);
		id(false);
		id(null);
		id(0);
		id(145);
		id( -145 );
		id(0.15461);
		id( -485.15461);
		id( 1e10 );
		id( -1e-10 );
		id( "" );
		id( "hello" );
		id( "he\n\r\t\\\\llo");

		deepId( {field: 4} );
		deepId( { test: { nested: null }} );
		var mix : Array<Dynamic> = [1, 2, 3, "str"];
		deepId( {array: mix} );

		eq( haxe.Json.parse('"\\u00E9"'), "é" );

		eq(haxe.Json.stringify(Math.POSITIVE_INFINITY), "null");
		eq(haxe.Json.stringify(Math.NEGATIVE_INFINITY), "null");
		eq(haxe.Json.stringify(Math.NaN), "null");

		return;
	}

	// TODO: test pretty-printing (also with objects with skipped function fields!)
	function testHaxeJson() {
		var str = haxe.format.JsonPrinter.print( { x : -4500, y : 1.456, a : ["hello", "wor'\"\n\t\rd"], b : function() {} } );
		str = str.substr(1, str.length - 2); // remove {}
		var parts = str.split(",");
		t( parts.remove('"x":-4500') );
		t( parts.remove('"y":1.456') );
		t( parts.remove('"a":["hello"') );
		t( parts.remove('"wor\'\\"\\n\\t\\rd"]') );
		eq( parts.join("#"), "" );

		function id(v:Dynamic,?pos:haxe.PosInfos) eq(haxe.format.JsonParser.parse(haxe.format.JsonPrinter.print(v)),v);
		function deepId(v:Dynamic) {
			var str = haxe.format.JsonPrinter.print(v);
			eq(haxe.format.JsonPrinter.print(haxe.format.JsonParser.parse(str)), str);
		}

		id(true);
		id(false);
		id(null);
		id(0);
		id(145);
		id( -145 );
		id(0.15461);
		id( -485.15461);
		id( 1e10 );
		id( -1e-10 );
		id( "" );
		id( "hello" );
		id( "he\n\r\t\\\\llo");

		deepId( {field: 4} );
		deepId( { test: { nested: null }} );
		var mix : Array<Dynamic> = [1, 2, 3, "str"];
		deepId( {array: mix} );

		eq( haxe.format.JsonParser.parse('"\\u00E9"'), "é" );
		//exc(() -> haxe.format.JsonParser.parse('{"""a": 1}'));
		exc( function() haxe.format.JsonParser.parse('{"""a": 1}'));

		eq(haxe.format.JsonPrinter.print(Math.POSITIVE_INFINITY), "null");
		eq(haxe.format.JsonPrinter.print(Math.NEGATIVE_INFINITY), "null");
		eq(haxe.format.JsonPrinter.print(Math.NaN), "null");
		eq(haxe.format.JsonPrinter.print(function() {}), "\"<fun>\"");
		eq(haxe.format.JsonPrinter.print({a: function() {}, b: 1}), "{\"b\":1}");
		// #11908
		eq(haxe.format.JsonPrinter.print({int64: haxe.Int64.parseString("123213213213213")}), "{\"int64\":123213213213213}");
	}

	function test3690() {
		var strJson = haxe.Json.stringify( { x : -4500, y : 1.456, a : ["hello", "wor'\"\n\t\rd"] } );
		var parsed : Dynamic = haxe.Json.parse( strJson );
		eq( parsed.x, -4500 );
		eq( parsed.y, 1.456 );
	}

	function test8593_stringifyNestedObjects() {
		var src = {
			one: {
				two: "three"
			}
		};
		eq('{"one":{"two":"three"}}', haxe.Json.stringify(src));
	}

	// Keys that Python stores under other names (keywords, a leading "__"): python.Lib.dictToAnon and
	// haxe.Json.parse didn't rename them, so they read as null and were written back as null.
	function testKeysNamedLikeKeywords() {
		var keys = ["pass", "from", "class", "in", "async", "nonlocal", "__private", "normal"];
		var src = "{" + [for (i => k in keys) '"$k":$i'].join(",") + "}";
		var o:Dynamic = haxe.Json.parse(src);
		for (i => k in keys)
			eq(i, Reflect.field(o, k));
		var fields = Reflect.fields(o);
		fields.sort(Reflect.compare);
		var sorted = keys.copy();
		sorted.sort(Reflect.compare);
		eq(sorted.join(","), fields.join(","));
		var again:Dynamic = haxe.Json.parse(haxe.Json.stringify(o));
		for (i => k in keys)
			eq(i, Reflect.field(again, k));
		var literal:Dynamic = {"from": 1, "pass": 2, "__private": 3};
		var back:Dynamic = haxe.Json.parse(haxe.Json.stringify(literal));
		eq(1, back.from);
		eq(2, back.pass);
		eq(3, Reflect.field(back, "__private"));
	}

	#if (!neko && (cpp && !cppia && !hxcpp_smart_strings))
	function test8228() {
		var strJson = haxe.Json.stringify("👽");
		t(strJson == '"👽"' || strJson == '"\\ud83d\\udc7d"');
		eq( haxe.Json.parse('"👽"'), "👽" );
		eq( haxe.Json.parse('"\\ud83d\\udc7d"'), "👽" );
	}
	#end
}
