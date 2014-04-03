
package python.lib;

import python.lib.Types.Dict;

extern class Json {


	public static function loads (s:String):Dict<String, Dynamic>;

	static function __init__ ():Void
	{
		python.Syntax.importAs("json", "python.lib.Json");
	}
}