package jvm;

import java.NativeString;

@:native("haxe.jvm.StringExt")
class StringExt {
	public static function fromCharCode(code:Int):String {
		var a = new java.NativeArray(1);
		a[0] = code;
		return new String(a, 0, 1);
	}

	public static function charAt(me:String, index:Int):String {
		if (index >= me.length || index < 0)
			return "";
		else
			return java.lang.Character._toString((cast me : NativeString).charAt(index));
	}

	public static function charCodeAt(me:String, index:Int):Null<Int> {
		if (index >= me.length || index < 0)
			return null;
		else
			return cast((cast me : NativeString).charAt(index), Int);
	}

	public static function indexOf(me:String, str:String, startIndex:Null<Int>) {
		return
			if (startIndex == null) (cast me : NativeString).indexOf(str)
			else (cast me : NativeString).indexOf(str, startIndex);
	}

	public static function lastIndexOf(me:String, str:String, ?startIndex:Int):Int {
		if (startIndex == null || startIndex > me.length || startIndex < 0) {
			startIndex = me.length - 1;
		}
		return (cast me : NativeString).lastIndexOf(str, startIndex);
	}

	public static function split(me:String, delimiter:String):Array<String> {
		var ret = [];
		if (delimiter.length == 0) {
			for (i in 0...me.length) {
				ret.push(me.charAt(i));
			}
		} else {
			var start = 0;
			var pos = me.indexOf(delimiter, start);
			while (pos >= 0) {
				ret.push((cast me : NativeString).substring(start, pos));
				start = pos + delimiter.length;
				pos = me.indexOf(delimiter, start);
			}
			ret.push((cast me : NativeString).substring(start));
		}
		return ret;
	}

	public static function substr(me:String, pos:Int, ?len:Int):String {
		var len:Int = len == null ? me.length : len;
		if (pos != 0 && len < 0) {
			return "";
		}
		if (pos < 0) {
			pos = me.length + pos;
			if (pos < 0) {
				pos = 0;
			}
		} else if (len < 0) {
			len = me.length + len - pos;
		}
		if (pos + len > me.length) {
			len = me.length - pos;
		}
		if (pos < 0 || len <= 0) {
			return "";
		}
		return (cast me : NativeString).substring(pos, pos + len);
	}

	public static function substring(me:String, startIndex:Int, ?endIndex:Int):String {
		var endIndex:Int = endIndex == null ? me.length : endIndex;
		if (endIndex < 0) {
			endIndex = 0;
		} else if (endIndex > me.length) {
			endIndex = me.length;
		}
		if (startIndex < 0) {
			startIndex = 0;
		} else if (startIndex > me.length) {
			startIndex = me.length;
		}

		if (startIndex > endIndex) {
			var tmp = startIndex;
			startIndex = endIndex;
			endIndex = tmp;
		}
		return (cast me : NativeString).substring(startIndex, endIndex);
	}

	public static function toLowerCase(me:String):String {
		return me.toLowerCase();
	}

	public static function toUpperCase(me:String):String {
		return me.toUpperCase();
	}
}
