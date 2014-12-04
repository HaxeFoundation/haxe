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
package cs.internal;
import cs.internal.Function;
private typedef NativeString = String;

@:keep @:nativeGen @:native("haxe.lang.StringExt") private class StringExt
{

	@:functionCode('
			if ( ((uint) index) >= me.Length)
				return "";
			else
				return new string(me[index], 1);
	')
	public static function charAt(me:NativeString, index:Int):NativeString
	{
		return null;
	}

	public static function charCodeAt(me:NativeString, index:Int):Null<Int>
	{
		if (cast(index,UInt) >= me.length)
			return null;
		else
			return cast me[index];
	}

	public static function indexOf(me:NativeString, str:NativeString, ?startIndex:Int):Int
	{
		var sIndex:Int = startIndex != null ? startIndex : 0;
		if (sIndex >= me.length)
			return -1;
		return @:privateAccess me.IndexOf(str, sIndex, cs.system.StringComparison.Ordinal);
	}

	@:functionCode('
			int sIndex = (startIndex.hasValue) ? (startIndex.@value) : (me.Length - 1);
			if (sIndex >= me.Length)
				sIndex = me.Length - 1;
			else if (sIndex < 0)
				return -1;

			//TestBaseTypes.hx@133 fix
			if (startIndex.hasValue)
			{
				for(int i = sIndex; i >= 0; i--)
				{
					bool found = true;
					for(int j = 0; j < str.Length; j++)
					{
						if(me[i + j] != str[j])
						{
							found = false;
							break;
						}
					}

					if (found)
						return i;
				}

				return -1;
			} else {
				return me.LastIndexOf(str, sIndex, System.StringComparison.Ordinal);
			}
	')
	public static function lastIndexOf(me:NativeString, str:NativeString, ?startIndex:Int):Int
	{
		return -1;
	}

	@:functionCode('
			string[] native;
			if (delimiter.Length == 0)
			{
				int len = me.Length;
				native = new string[len];
				for (int i = 0; i < len; i++)
					native[i] = new string(me[i], 1);
			} else {
				native = me.Split(new string[] { delimiter }, System.StringSplitOptions.None);
			}
			return new Array<object>(native);
	')
	public static function split(me:NativeString, delimiter:NativeString):Array<NativeString>
	{
		return null;
	}

	@:functionCode('
			int meLen = me.Length;
			int targetLen = meLen;
			if (len.hasValue)
			{
				targetLen = len.@value;
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

			return me.Substring(pos, targetLen);
	')
	public static function substr(me:NativeString, pos:Int, ?len:Int):NativeString
	{
		return null;
	}

	@:functionCode('
		int endIdx;
		int len = me.Length;
		if ( !endIndex.hasValue ) {
			endIdx = len;
		} else if ( (endIdx = endIndex.@value) < 0 ) {
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

		return me.Substring(startIndex, endIdx - startIndex);
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
			return me.ToLowerInvariant();
	')
	public static function toLowerCase(me:NativeString):NativeString
	{
		return null;
	}

	@:functionCode('
			return me.ToUpperInvariant();
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
			return new string( (char) code, 1 );
	')
	public static function fromCharCode(code:Int):NativeString
	{
		return null;
	}
}

@:keep @:nativeGen @:native('haxe.lang.StringRefl') class StringRefl
{
	public static var fields = ["length", "toUpperCase", "toLowerCase", "charAt", "charCodeAt", "indexOf", "lastIndexOf", "split", "substr", "substring"];

	public static function handleGetField(str:NativeString, f:NativeString, throwErrors:Bool):Dynamic
	{
		switch(f)
		{
			case "length": return str.length;
			case "toUpperCase", "toLowerCase", "charAt", "charCodeAt", "indexOf", "lastIndexOf", "split", "substr", "substring":
				return new Closure(str, f, 0);
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
