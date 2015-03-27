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
        #if php
        // php's haxe.Utf8 uses mbstring
        if (untyped __call__("extension_loaded", "mbstring")) {
        #end

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

        eq(haxe.format.JsonPrinter.print(Math.POSITIVE_INFINITY), "null");
        eq(haxe.format.JsonPrinter.print(Math.NEGATIVE_INFINITY), "null");
        eq(haxe.format.JsonPrinter.print(Math.NaN), "null");
        eq(haxe.format.JsonPrinter.print(function() {}), "\"<fun>\"");
        eq(haxe.format.JsonPrinter.print({a: function() {}, b: 1}), "{\"b\":1}");

        #if php
        }
        #end
    }

	function test3690() {
		var strJson = haxe.Json.stringify( { x : -4500, y : 1.456, a : ["hello", "wor'\"\n\t\rd"] } );
		var parsed : Dynamic = haxe.Json.parse( strJson );
		eq( parsed.x, -4500 );
		eq( parsed.y, 1.456 );
	}
}