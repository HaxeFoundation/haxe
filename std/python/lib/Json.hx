
package python.lib;

import python.KwArgs;
import python.lib.Dict;
import python.lib.Tuple.Tup2;

@:pythonImport("json")
extern class Json {


	public static function loads (
		s:String,
		encoding:Null<String> = null,
		cls : Null<Dynamic> = null,
		object_hook:Null<Dict<String, Dynamic>->Dynamic> = null
		):Dict<String, Dynamic>;
	public static function dumps (x:Dynamic, skipkeys:Bool=false, ensure_ascii:Bool=true, check_circular:Bool=true,
		allow_nan:Bool=true,
		cls:Null<Dynamic> = null, indent:Null<String> = null,
		separators:Null<Tup2<String,String>>, /*default*/def:Null<Dynamic->String> = null, sort_keys:Bool=false, kw:KwArgs = null):String;

}