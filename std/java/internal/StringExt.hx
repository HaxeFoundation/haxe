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
package java.internal;
import java.internal.Function;

private typedef NativeString = String;

@:keep @:nativeGen @:native("haxe.lang.StringExt") private class StringExt
{

	@:functionCode('
			if ( index >= me.length() || index < 0 )
				return "";
			else
				return java.lang.Character.toString(me.charAt(index));
	')
	public static function charAt(me:NativeString, index:Int):NativeString
	{
		return null;
	}

	@:functionCode('
			if ( index >= me.length() || index < 0 )
				return null;
			else
				return (int) me.charAt(index);
	')
	public static function charCodeAt(me:NativeString, index:Int):Null<Int>
	{
		return null;
	}

	@:functionCode('
			int sIndex = (startIndex != null ) ? (haxe.lang.Runtime.toInt(startIndex)) : 0;
			if (sIndex >= me.length() || sIndex < 0)
				return -1;
			return me.indexOf(str, sIndex);
	')
	public static function indexOf(me:NativeString, str:NativeString, ?startIndex:Int):Int
	{
		return -1;
	}

	@:functionCode('
			int sIndex = (startIndex != null ) ? (haxe.lang.Runtime.toInt(startIndex)) : (me.length() - 1);
			if (sIndex > me.length() || sIndex < 0)
				sIndex = me.length() - 1;
			else if (sIndex < 0)
				return -1;
			return me.lastIndexOf(str, sIndex);
	')
	public static function lastIndexOf(me:NativeString, str:NativeString, ?startIndex:Int):Int
	{
		return -1;
	}

	@:functionCode('
			Array<java.lang.String> ret = new Array<java.lang.String>();

			int slen = delimiter.length();
			if (slen == 0)
			{
				int len = me.length();
				for (int i = 0; i < len; i++)
				{
					ret.push(me.substring(i, i + 1));
				}
			} else {
				int start = 0;
				int pos = me.indexOf(delimiter, start);

				while (pos >= 0)
				{
					ret.push(me.substring(start, pos));

					start = pos + slen;
					pos = me.indexOf(delimiter, start);
				}

				ret.push(me.substring(start));
			}
			return ret;
	')
	public static function split(me:NativeString, delimiter:NativeString):Array<NativeString>
	{
		return null;
	}

	@:functionCode('
			int meLen = me.length();
			int targetLen = meLen;
			if (len != null)
			{
				targetLen = haxe.lang.Runtime.toInt(len);
				if (targetLen == 0)
					return "";
				if( pos != 0 && targetLen < 0 ){
					return "";
				}
			}

			if( pos < 0 ){
				pos = meLen + pos;
				if( pos < 0 ) pos = 0;
			} else if( targetLen < 0 ){
				targetLen = meLen + targetLen - pos;
			}

			if( pos + targetLen > meLen ){
				targetLen = meLen - pos;
			}

			if ( pos < 0 || targetLen <= 0 ) return "";

			return me.substring(pos, pos + targetLen);
	')
	public static function substr(me:NativeString, pos:Int, ?len:Int):NativeString
	{
		return null;
	}

	@:functionCode('
		int endIdx;
		int len = me.length();
		if ( endIndex == null) {
			endIdx = len;
		} else if ( (endIdx = haxe.lang.Runtime.toInt(endIndex)) < 0 ) {
			endIdx = 0;
		} else if ( endIdx > len ) {
			endIdx = len;
		}

		if ( startIndex < 0 ) {
			startIndex = 0;
		} else if ( startIndex > len ) {
			startIndex = len;
		}

		if ( startIndex > endIdx ) {
			int tmp = startIndex;
			startIndex = endIdx;
			endIdx = tmp;
		}

		return me.substring(startIndex, endIdx);

	')
	public static function substring(me:NativeString, startIndex:Int, ?endIndex:Int):NativeString
	{
		return null;
	}

	public static function toString(me:NativeString):NativeString
	{
		return me;
	}

	@:functionCode('
			return me.toLowerCase();
	')
	public static function toLowerCase(me:NativeString):NativeString
	{
		return null;
	}

	@:functionCode('
			return me.toUpperCase();
	')
	public static function toUpperCase(me:NativeString):NativeString
	{
		return null;
	}

	public static function toNativeString(me:NativeString):NativeString
	{
		return me;
	}

	@:functionCode('
		return java.lang.Character.toString( (char) code );
	')
	public static function fromCharCode(code:Int):NativeString
	{
		return null;
	}
}

@:keep @:nativeGen @:native('haxe.lang.StringRefl') private class StringRefl
{
	public static var fields = ["length", "toUpperCase", "toLowerCase", "charAt", "charCodeAt", "indexOf", "lastIndexOf", "split", "substr", "substring"];

	public static function handleGetField(str:NativeString, f:NativeString, throwErrors:Bool):Dynamic
	{
		switch(f)
		{
			case "length": return str.length;
			case "toUpperCase", "toLowerCase", "charAt", "charCodeAt", "indexOf", "lastIndexOf", "split", "substr", "substring":
				return new Closure(str, f);
			default:
				if (throwErrors)
					throw "Field not found: '" + f + "' in String";
				else
					return null;
		}
	}

	public static function handleCallField(str:NativeString, f:NativeString, args:Array<Dynamic>):Dynamic
	{
		var _args:Array<Dynamic> = [str];
		if (args == null)
			args = _args;
		else
			args = _args.concat(args);

		return Runtime.slowCallField(StringExt, f, args);
	}
}

@:keep @:native('haxe.lang.NativeString') private extern class JavaString
{
	//name collides with Haxe's
	function _charAt(idx:Int):java.StdTypes.Char16;
	function codePointAt(idx:Int):Int;
	function codePointBefore(idx:Int):Int;
	function codePointCount(begin:Int, end:Int):Int;
	function offsetByCodePoints(index:Int, codePointOffset:Int):Int;
	function getChars(srcBegin:Int, srcEnd:Int, dst:java.NativeArray<java.StdTypes.Char16>, dstBegin:Int):Void;


	function startsWith(prefix:String):Bool;
	function endsWith(suffix:String):Bool;
	function _indexOf(str:String, fromIndex:Int):Int;
	function _lastIndexOf(str:String, fromIndex:Int):Int;
	function _substring(begin:Int, end:Int):String;
	function replace(old:String, nw:String):String;
	function _split(regex:String):java.NativeArray<String>;
	function trim():String;
}
