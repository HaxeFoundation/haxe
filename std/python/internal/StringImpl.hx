package python.internal;

import python.lib.Builtin;

@:keep
@:native("HxString")
class StringImpl {

	public static function split (s:String, d:String) {
		return if (d == "") Builtin.list(s) else Macros.callField(s, "split", d);
	}

	public static function charCodeAt(s:String, index:Int) {
		return if (s == null || s.length == 0 || index < 0 || index >= s.length) null else untyped ord(untyped __python_array_get__(s, index));
	}
	public static inline function charAt(s:String, index:Int) {
		return if (index < 0 || index >= s.length) "" else untyped __python_array_get__(s,index);
	}
	public static inline function lastIndexOf(s:String, str:String, ?startIndex:Int):Int {
		if (startIndex == null) {
			return (untyped s.rfind)(str, 0, s.length);
		} else {

			var i = (untyped s.rfind)(str, 0, startIndex+1);
			var startLeft = i == -1 ? Math.max(0,startIndex+1-str.length) : i+1;
			var check = (untyped s.find)(str, startLeft, s.length);
			if (check > i && check <= startIndex) {
				return check;
			} else {
				return i;
			}
		}
	}

	public static function toUpperCase (s:String) {
		return Macros.callField(s, "upper");
	}

	public static function toLowerCase (s:String) {
		return Macros.callField(s, "lower");
	}
	public static function indexOf (s:String, str:String, ?startIndex:Int) {
		if (startIndex == null)
			return Macros.callField(s, "find", str);
		else
			return Macros.callField(s, "find", str, startIndex);
	}

	public static function toString (s:String) {
		return s;
	}

	public static function get_length (s:String) {
		return python.lib.Builtin.len(s);
	}

	public static inline function fromCharCode( code : Int ) : String {
		#if doc_gen
		return "";
		#else
		var c = code;
		return (untyped (''.join)(Builtin.map(Builtin.chr, cast [c])):String); // TODO: check cast
		#end
	}

	public static function substring( s:String, startIndex : Int, ?endIndex : Int ) : String {
		if (startIndex < 0) startIndex = 0;
		if (endIndex == null) {
			return untyped __python__("s[startIndex:]");
		} else {
			if (endIndex < 0) endIndex = 0;
			if (endIndex < startIndex) {

				return untyped __python__("s[endIndex:startIndex]");
			} else {

				return untyped __python__("s[startIndex:endIndex]");
			}

		}
	}

	public static function substr( s:String, startIndex : Int, ?len : Int ) : String {
		if (len == null) {
			return untyped __python__("s[startIndex:]");
		} else {
			if (len == 0) return "";
			return untyped __python__("s[startIndex:startIndex+len]");
		}

	}
}