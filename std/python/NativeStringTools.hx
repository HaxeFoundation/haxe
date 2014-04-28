
package python;

import python.lib.Bytes;
import python.lib.Tuple;

class NativeStringTools {

	public static function format (s:String, args:Array<Dynamic>):String
	{
		return python.Syntax.field(s, "format")(python.Syntax.varArgs(args));
	}

	@:keep public static inline function encode(s:String, encoding:String="utf-8", errors:String="strict"):Bytes {
		return (python.Syntax.callField(s, "encode", encoding, errors):Bytes);
	}

	public static inline function contains(s:String, e:String):Bool {
		return python.Syntax.isIn(e,s);
	}

	public static inline function strip(s:String, ?chars:String):String
	{
		return python.Syntax.field(s, "strip")(chars);
	}

	public static inline function rpartition (s:String, sep:String):Tup3<String, String, String>
	{
		return python.Syntax.field(s, "rpartition")(sep);
	}

}