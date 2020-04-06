/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package python.lib;

import python.Tuple;

private abstract Choice<A, B>(Dynamic) {
	@:from public static inline function fromA<A, B>(x:A):Choice<A, B>
		return cast x;

	@:from public static inline function fromB<A, B>(x:B):Choice<A, B>
		return cast x;
}

typedef Pattern = Choice<String, Regex>;
typedef Repl = Choice<String, MatchObject->String>;

extern class MatchObject {
	var pos(default, null):Int;
	var endpos(default, null):Int;
	var lastindex(default, null):Int;
	var lastgroup(default, null):Int;
	var re(default, null):Regex;
	var string(default, null):String;

	function expand(template:String):String;

	@:overload(function(x:String):String {})
	function group(?i:Int = 0):String;

	function groups(defaultVal:String = null):Tuple<String>;
	function groupdict(defaultVal:Dict<String, String> = null):Dict<String, String>;

	@:overload(function(x:String):Int {})
	function start(?i:Int = 0):Int;

	@:overload(function(x:String):Int {})
	function end(?i:Int = 0):Int;

	function span(?i:Int):Tuple2<Int, Int>;

	inline function groupById(s:String):String {
		return group(s);
	}

	inline function startById(s:String):Int {
		return start(s);
	}

	inline function endById(s:String):Int {
		return end(s);
	}
}

private class RegexHelper {
	public static function findallDynamic(r:Regex, string:String, ?pos:Int, ?endpos:Int):Array<Dynamic> {
		if (endpos == null) {
			if (pos == null) {
				return python.Syntax.field(r, "findall")(string);
			} else {
				return python.Syntax.field(r, "findall")(string, pos);
			}
		} else {
			return python.Syntax.field(r, "findall")(string, pos, endpos);
		}
	}
}

extern class Regex {
	function search(string:String, pos:Int = 0, ?endpos:Int):Null<MatchObject>;
	function match(string:String, pos:Int = 0, ?endpos:Int):Null<MatchObject>;

	function split(string:String, maxsplit:Int = 0):Array<String>;

	inline function findallString(string:String, ?pos:Int, ?endpos:Int):Array<String> {
		return cast this.findallDynamic(string, pos, endpos);
	}

	inline function findallDynamic(string:String, ?pos:Int, ?endpos:Int):Array<Dynamic> {
		return RegexHelper.findallDynamic(this, string, pos, endpos);
	}

	inline function findallTuple(string:String, ?pos:Int, ?endpos:Int):Array<Tuple<String>> {
		return cast this.findallDynamic(string, pos, endpos);
	}

	inline function findallArray(string:String, ?pos:Int, ?endpos:Int):Array<Array<String>> {
		return findallTuple(string, pos, endpos).map(function(t) return t.toArray());
	}

	function finditer(string:String, ?pos:Int, ?endpos:Int):NativeIterator<MatchObject>;

	function sub(repl:Repl, string:String, count:Int = 0):String;
	function subn(repl:Repl, string:String, count:Int = 0):String;

	var flags(default, null):Int;
	var groups(default, null):Int;
	var groupindex(default, null):Dict<String, Int>;
	var pattern(default, null):String;
}

@:pythonImport("re")
extern class Re {
	static var A:Int;
	static var ASCII:Int;
	static var DEBUG:Int;
	static var I:Int;
	static var IGNORECASE:Int;

	static var L:Int;
	static var LOCALE:Int;

	static var M:Int;
	static var MULTILINE:Int;

	static var S:Int;
	static var DOTALL:Int;

	static var X:Int;
	static var VERBOSE:Int;

	static var U:Int;
	static var UNICODE:Int;

	static function compile(pattern:String, ?flags:Int = 0):Regex;

	static function match(pattern:Pattern, string:String, flags:Int = 0):Null<MatchObject>;

	static function search(pattern:Pattern, string:String, flags:Int = 0):Null<MatchObject>;

	static function split(pattern:Pattern, string:String, maxsplit:Int = 0, flags:Int = 0):Array<String>;

	static inline function findallDynamic(pattern:Pattern, string:String, flags:Int = 0):Array<Dynamic> {
		return python.Syntax.field(pattern, "findall")(string, flags);
	}

	static inline function findallString(pattern:Pattern, string:String, flags:Int = 0):Array<String> {
		return python.Syntax.field(pattern, "findall")(string, flags);
	}

	static inline function findallTuple(pattern:Pattern, string:String, flags:Int = 0):Array<Tuple<String>> {
		return python.Syntax.field(pattern, "findall")(string, flags);
	}

	static inline function findallArray(pattern:Pattern, string:String, flags:Int = 0):Array<Array<String>> {
		return findallTuple(pattern, string, flags).map(function(t) return t.toArray());
	}

	static function finditer(pattern:Pattern, string:String, flags:Int = 0):NativeIterator<MatchObject>;

	@:overload(function(pattern:Pattern, repl:String, string:String, ?count:Int = 0, ?flags:Int = 0):String {})
	static function sub(pattern:Pattern, repl:MatchObject->String, string:String, ?count:Int = 0, ?flags:Int = 0):String;

	static function subn(pattern:Pattern, repl:Repl, string:String, count:Int = 0, flags:Int = 0):String;

	static function escape(string:String):String;

	static function purge():Void;
}
