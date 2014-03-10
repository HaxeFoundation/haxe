
package python.lib.datetime;

extern class TzInfo {

	
	static function __init__ ():Void 
	{
		python.Macros.importFromAs("datetime", "tzinfo", "python.lib.datetime.TzInfo");
	}
}