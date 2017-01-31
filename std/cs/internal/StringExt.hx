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
package cs.internal;
import cs.internal.Function;
private typedef NativeString = cs.system.String;

@:keep @:nativeGen @:native("haxe.lang.StringExt") class StringExt
{
	@:readOnly static var empty(default,never) = new NativeString(cast 0,0);

	public static function charAt(me:NativeString, index:Int):NativeString
	{
		if (cast(index,UInt) >= me.Length)
			return empty;
		else
			return new NativeString(me[index], 1);
	}

	public static function charCodeAt(me:NativeString, index:Int):Null<Int>
	{
		if (cast(index,UInt) >= me.Length)
			return null;
		else
			return cast(me[index], Int);
	}

	public static function indexOf(me:NativeString, str:String, ?startIndex:Int):Int
	{
		var sIndex:Int = startIndex != null ? startIndex : 0;
		if (sIndex >= me.Length)
			return -1;
		return @:privateAccess me.IndexOf(str, sIndex, cs.system.StringComparison.Ordinal);
	}

	public static function lastIndexOf(me:NativeString, str:NativeString, ?startIndex:Int):Int
	{
		var sIndex:Int = startIndex == null ? me.Length - 1 : startIndex;
		if (sIndex >= me.Length)
			sIndex = me.Length - 1;
		else if (sIndex < 0)
			return -1;

		//TestBaseTypes.hx@133 fix
		if (startIndex != null)
		{
			var i = sIndex + 1;
			while (i --> 0)
			{
				var found = true;
				for (j in 0...str.Length)
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
			return me.LastIndexOf(untyped str, sIndex, cs.system.StringComparison.Ordinal);
		}
		return -1;
	}

	public static function split(me:NativeString, delimiter:NativeString):Array<String>
	{
		var native:NativeArray<String>;
		if (delimiter.Length == 0)
		{
			var len = me.Length;
			native = new NativeArray(len);
			for (i in 0...len)
				native[i] = untyped new NativeString(me[i],1);
		} else {
			var str = new NativeArray<String>(1);
			str[0] = cast delimiter;

			native = me.Split(str, cs.system.StringSplitOptions.None);
		}

		return cs.Lib.array(native);
	}

	public static function substr(me:NativeString, pos:Int, ?len:Int):String
	{
		var meLen = me.Length;
		var targetLen = meLen;
		if (len != null)
		{
			targetLen = len;
			if (targetLen == 0 || (pos != 0 && targetLen < 0))
				return "";
		}

		if (pos < 0)
		{
			pos = meLen + pos;
			if (pos < 0) pos = 0;
		} else if (targetLen < 0) {
			targetLen = meLen + targetLen - pos;
		}

		if (pos + targetLen > meLen)
		{
			targetLen = meLen - pos;
		}

		if (pos < 0 || targetLen <= 0) return "";

		return me.Substring(pos, targetLen);
	}

	public static function substring(me:NativeString, startIndex:Int, ?endIndex:Int):String
	{
		var len = me.Length;
		var endIdx:Int;
		if (endIndex == null)
			endIdx = len;
		else if ( (endIdx = endIndex) < 0 )
			endIdx = 0;
		else if (endIdx > len)
			endIdx = len;

		if (startIndex < 0)
			startIndex = 0;
		else if (startIndex > len)
			startIndex = len;

		if (startIndex > endIdx)
		{
			var tmp = startIndex;
			startIndex = endIdx;
			endIdx = tmp;
		}

		return me.Substring(startIndex, endIdx - startIndex);
	}

	public static function toString(me:NativeString):NativeString
	{
		return me;
	}

	public static function toLowerCase(me:NativeString):String
	{
		return me.ToLowerInvariant();
	}

	public static function toUpperCase(me:NativeString):String
	{
		return me.ToUpperInvariant();
	}

	public static function toNativeString(me:NativeString):NativeString
	{
		return me;
	}

	public static function fromCharCode(code:Int):NativeString
	{
		return new NativeString( cast(code,cs.StdTypes.Char16), 1 );
	}
}

@:keep @:nativeGen @:native('haxe.lang.StringRefl') class StringRefl
{
	public static var fields = ["length", "toUpperCase", "toLowerCase", "charAt", "charCodeAt", "indexOf", "lastIndexOf", "split", "substr", "substring"];

	public static function handleGetField(str:String, f:String, throwErrors:Bool):Dynamic
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

	public static function handleCallField(str:NativeString, f:String, args:Array<Dynamic>):Dynamic
	{
		var _args:Array<Dynamic> = [str];
		if (args == null)
			args = _args;
		else
			args = _args.concat(args);

		return Runtime.slowCallField(StringExt, f, args);
	}
}
