package python.internal;

import python.internal.Internal;
import python.internal.HxBuiltin;

@:keep
@:native("HxString")
class StringImpl {

	public static inline function split (s:String, d:String) {
		return if (d == "") Syntax.field(HxBuiltin, "list")(s) else Syntax.callField(s, "split", d);
	}

	public static function charCodeAt(s:String, index:Int) {
		return
			if (s == null || s.length == 0 || index < 0 || index >= s.length) null
			else Syntax.callField(HxBuiltin, "ord", Syntax.arrayAccess(s, index));
	}

	public static inline function charAt(s:String, index:Int) {
		return if (index < 0 || index >= s.length) "" else Syntax.arrayAccess(s,index);
	}

	public static inline function lastIndexOf(s:String, str:String, ?startIndex:Int):Int {
		if (startIndex == null) {
			return Syntax.callField(s, "rfind", str, 0, s.length);
		} else {

			var i = Syntax.callField(s, "rfind", str, 0, startIndex+1);
			var startLeft = i == -1 ? Syntax.callField(HxBuiltin, "max", 0,startIndex+1-str.length) : i+1;
			var check = Syntax.callField(s,"find", str, startLeft, s.length);
			if (check > i && check <= startIndex) {
				return check;
			} else {
				return i;
			}
		}
	}

	public static inline function toUpperCase (s:String) {
		return Syntax.callField(s, "upper");
	}

	public static inline function toLowerCase (s:String) {
		return Syntax.callField(s, "lower");
	}
	public static inline function indexOf (s:String, str:String, ?startIndex:Int) {
		if (startIndex == null)
			return Syntax.callField(s, "find", str);
		else
			return Syntax.callField(s, "find", str, startIndex);
	}

	public static inline function toString (s:String) {
		return s;
	}

	public static inline function get_length (s:String) {
		return Syntax.field(HxBuiltin, "len")(s);
	}

	public static inline function fromCharCode( code : Int ) : String {
		#if doc_gen
		return "";
		#else
		var c = code;
		return Syntax.callField('', "join",
			Syntax.callField(HxBuiltin, "map", Syntax.field(HxBuiltin, "chr"), [c])); // TODO: check cast
		#end
	}

	public static function substring( s:String, startIndex : Int, ?endIndex : Int ) : String {
		if (startIndex < 0) startIndex = 0;
		if (endIndex == null) {
			return Syntax.arrayAccessWithTrailingColon(s, startIndex);
		} else {
			if (endIndex < 0) endIndex = 0;
			if (endIndex < startIndex) {

				return Syntax.arrayAccess(s, endIndex, startIndex);
			} else {

				return Syntax.arrayAccess(s, startIndex, endIndex);
			}
		}
	}

	public static function substr( s:String, startIndex : Int, ?len : Int ) : String {
		if (len == null) {
			return Syntax.arrayAccessWithTrailingColon(s, startIndex);
		} else {
			if (len == 0) return "";
			return Syntax.arrayAccess(s, startIndex, startIndex+len);
		}

	}
}