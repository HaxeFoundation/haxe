/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package java.util.regex;
import java.NativeArray;

extern class Pattern
{
	static function compile(regex:String, flags:Int):Pattern;

	function matcher(input:String):Matcher;
	function split(input:String):NativeArray<String>;

	static var CANON_EQ(default, null):Int;
	static var CASE_INSENSITIVE(default, null):Int;
	static var COMMENTS(default, null):Int;
	static var DOTALL(default, null):Int;
	static var MULTILINE(default, null):Int;
	static var UNICODE_CASE(default, null):Int;
	static var UNIX_LINES (default, null):Int;
}


extern interface MatchResult
{
	@:overload(function(group:Int):Int {})
	function end():Int;

	@:overload(function():String {})
	function group(group:Int):String;

	function groupCount():Int;

	@:overload(function(group:Int):Int {})
	function start():Int;
}

extern class Matcher implements MatchResult
{
	function reset(input:String):Matcher;

	@:overload(function(group:Int):Int {})
	function end():Int;

	@:overload(function():String {})
	function group(group:Int):String;

	function groupCount():Int;

	@:overload(function(group:Int):Int {})
	function start():Int;

	@:overload(function(startPos:Int):Bool{})
	function find():Bool;

	function replaceAll(replacement:String):String;

	function replaceFirst(replacement:String):String;

	function pattern():Pattern;
}
