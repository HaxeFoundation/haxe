/*
 * Copyright (C)2005-2017 Haxe Foundation
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


private abstract Choice <A,B>(Dynamic) {
	@:from public static inline function fromA <A,B>(x:A):Choice<A,B> return cast x;
	@:from public static inline function fromB <A,B>(x:B):Choice<A,B> return cast x;
}

typedef TODO = Dynamic;

typedef Pattern = Choice<String, Regex>;

typedef Repl = Choice<String, MatchObject->String>;




extern class MatchObject
{

	public var pos(default, null):Int;
	public var endpos(default, null):Int;
	public var lastindex(default, null):Int;
	public var lastgroup(default, null):Int;
	public var re(default, null):Regex;
	public var string(default, null):String;

	public function expand(template:String):String;

	@:overload(function (x:String):String {})
	public function group(?i:Int = 0):String;

	public function groups(defaultVal:String = null):Tuple<String>;
	public function groupdict(defaultVal:Dict<String, String> = null):Dict<String, String>;

	@:overload(function (x:String):Int {})
	public function start (?i:Int = 0):Int;

	@:overload(function (x:String):Int {})
	public function end (?i:Int = 0):Int;

	public function span (?i:Int):Tuple2<Int, Int>;


	public inline function groupById(s:String):String {
		return group(s);
	}

	public inline function startById(s:String):Int {
		return start(s);
	}

	public inline function endById(s:String):Int {
		return end(s);
	}

}

private class RegexHelper {
	public static function findallDynamic(r:Regex, string:String, ?pos:Int, ?endpos:Int):Array<Dynamic>
	{
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

extern class Regex
{
	public function search(string:String, pos:Int = 0, ?endpos:Int):Null<MatchObject>;
	public function match(string:String, pos:Int = 0, ?endpos:Int):Null<MatchObject>;

	public function split(string:String, maxsplit:Int=0):Array<String>;

	public inline function findallString(string:String, ?pos:Int, ?endpos:Int):Array<String>
	{
		return cast this.findallDynamic(string, pos, endpos);
	}


	public inline function findallDynamic(string:String, ?pos:Int, ?endpos:Int):Array<Dynamic>
	{
		return RegexHelper.findallDynamic(this, string, pos, endpos);
	}

	public inline function findallTuple(string:String, ?pos:Int, ?endpos:Int):Array<Tuple<String>> {

		return cast this.findallDynamic(string, pos, endpos);
	}

	public inline function findallArray(string:String, ?pos:Int, ?endpos:Int):Array<Array<String>>
	{
		return findallTuple(string, pos, endpos).map(function (t) return t.toArray());
	}

	public function finditer(string:String, ?pos:Int, ?endpos:Int):NativeIterator<MatchObject>;

	public function sub(repl:Repl, string:String, count:Int=0):String;
	public function subn(repl:Repl, string:String, count:Int=0):String;

	public var flags(default, null):Int;
	public var groups(default, null):Int;
	public var groupindex(default, null):Dict<String, Int>;
	public var pattern(default, null):String;
}


@:pythonImport("re")
extern class Re
{

	public static var A:Int;
	public static var ASCII:Int;
	public static var DEBUG:Int;
	public static var I:Int;
	public static var IGNORECASE:Int;

	public static var L:Int;
	public static var LOCALE:Int;

	public static var M:Int;
	public static var MULTILINE:Int;

	public static var S:Int;
	public static var DOTALL:Int;

	public static var X:Int;
	public static var VERBOSE:Int;

	public static var U:Int;
	public static var UNICODE:Int;



	public static function compile (pattern:String, ?flags:Int = 0):Regex;

	public static function match (pattern:Pattern, string:String, flags:Int = 0):Null<MatchObject>;

	public static function search (pattern:Pattern, string:String, flags:Int = 0):Null<MatchObject>;

	public static function split(pattern:Pattern, string:String, maxsplit:Int=0, flags:Int=0):Array<String>;

	public static inline function findallDynamic(pattern:Pattern, string:String, flags:Int=0):Array<Dynamic>
	{
		return python.Syntax.field(pattern, "findall")(string, flags);
	}


	public static inline function findallString(pattern:Pattern, string:String,	flags:Int=0):Array<String>
	{
		return python.Syntax.field(pattern, "findall")(string, flags);
	}

	public static inline function findallTuple(pattern:Pattern, string:String, flags:Int=0):Array<Tuple<String>>
	{
		return python.Syntax.field(pattern, "findall")(string, flags);
	}

	public static inline function findallArray(pattern:Pattern, string:String, flags:Int=0):Array<Array<String>>
	{
		return findallTuple(pattern, string,flags).map(function (t) return t.toArray());
	}

	public static function finditer(pattern:Pattern, string:String,   flags:Int=0):NativeIterator<MatchObject>;


	@:overload(function (pattern:Pattern, repl:String, string:String,  ?count:Int=0, ?flags:Int=0):String {})
	public static function sub(pattern:Pattern, repl:MatchObject->String, string:String,  ?count:Int=0, ?flags:Int=0):String;

	public static function subn(pattern:Pattern, repl:Repl, string:String, count:Int=0, flags:Int=0):String;

	public static function escape(string:String):TODO;

	public static function purge():Void;
}