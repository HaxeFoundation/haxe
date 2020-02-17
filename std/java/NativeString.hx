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

package java;

import haxe.Int64;
import haxe.extern.Rest;
import java.StdTypes.Int8;
import java.StdTypes.Char16;
import java.lang.CharSequence;
import java.util.Locale;

@:native("java.lang.String")
extern class NativeString {
	function charAt(index:Int):Char16;
	function codePointAt(index:Int):Int;
	function codePointBefore(index:Int):Int;
	function codePointCount(beginIndex:Int, endIndex:Int):Int;
	function compareTo(anotherString:String):Int;
	function compareToIgnoreCase(str:String):Int;
	function concat(str:String):String;
	function contains(s:CharSequence):Bool;
	@:overload function contentEquals(cs:CharSequence):Bool;
	@:overload function contentEquals(sb:java.lang.StringBuffer):Bool;
	@:overload static function copyValueOf(data:NativeArray<Char16>):String;
	@:overload static function copyValueOf(data:NativeArray<Char16>, offset:Int, count:Int):String;
	function endsWith(suffix:String):Bool;
	function equals(anObject:Dynamic):Bool;
	function equalsIgnoreCase(anotherString:String):Bool;
	@:overload static function format(l:Locale, format:String, args:Rest<Dynamic>):String;
	@:overload static function format(format:String, args:Rest<Dynamic>):String;
	@:overload function getBytes():NativeArray<Int8>;
	@:overload function getBytes(charset:java.nio.charset.Charset):NativeArray<Int8>;
	@:deprecated @:overload function getBytes(srcBegin:Int, srcEnd:Int, dst:NativeArray<Int8>, dstBegin:Int):Void;
	@:overload function getBytes(charsetName:String):NativeArray<Int8>;
	function getChars(srcBegin:Int, srcEnd:Int, dst:NativeArray<Char16>, dstBegin:Int):Void;
	function hashCode():Int;
	@:overload function indexOf(ch:Int):Int;
	@:overload function indexOf(ch:Int, fromIndex:Int):Int;
	@:overload function indexOf(str:String):Int;
	@:overload function indexOf(str:String, fromIndex:Int):Int;
	function intern():String;
	function isEmpty():Bool;
	@:overload function lastIndexOf(ch:Int):Int;
	@:overload function lastIndexOf(ch:Int, fromIndex:Int):Int;
	@:overload function lastIndexOf(str:String):Int;
	@:overload function lastIndexOf(str:String, fromIndex:Int):Int;
	function length():Int;
	function matches(regex:String):Bool;
	function offsetByCodePoints(index:Int, codePointOffset:Int):Int;
	@:overload function regionMatches(ignoreCase:Bool, toffset:Int, other:String, ooffset:Int, len:Int):Bool;
	@:overload function regionMatches(toffset:Int, other:String, ooffset:Int, len:Int):Bool;
	@:overload function replace(oldChar:Char16, newChar:Char16):String;
	@:overload function replace(target:CharSequence, replacement:CharSequence):String;
	function replaceAll(regex:String, replacement:String):String;
	function replaceFirst(regex:String, replacement:String):String;
	@:overload function split(regex:String):NativeArray<String>;
	@:overload function split(regex:String, limit:Int):NativeArray<String>;
	@:overload function startsWith(prefix:String):Bool;
	@:overload function startsWith(prefix:String, toffset:Int):Bool;
	function subSequence(beginIndex:Int, endIndex:Int):CharSequence;
	@:overload function substring(beginIndex:Int):String;
	@:overload function substring(beginIndex:Int, endIndex:Int):String;
	function toCharArray():NativeArray<Char16>;
	@:overload function toLowerCase():String;
	@:overload function toLowerCase(locale:Locale):String;
	function toString():String;
	@:overload function toUpperCase():String;
	@:overload function toUpperCase(locale:Locale):String;
	function trim():String;
	@:overload static function valueOf(b:Bool):String;
	@:overload static function valueOf(c:Char16):String;
	@:overload static function valueOf(data:NativeArray<Char16>):String;
	@:overload static function valueOf(data:NativeArray<Char16>, offset:Int, count:Int):String;
	@:overload static function valueOf(d:Float):String;
	@:overload static function valueOf(f:String):String;
	@:overload static function valueOf(i:Int):String;
	@:overload static function valueOf(l:haxe.Int64):String;
	@:overload static function valueOf(obj:java.lang.Object):String;
}
