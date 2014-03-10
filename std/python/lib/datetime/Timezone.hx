
package python.lib.datetime;

extern class Timezone extends TzInfo {

	public static var utc(default, never):TzInfo;
	static function __init__ ():Void 
	{
		python.Macros.importFromAs("datetime", "timezone", "python.lib.datetime.Timezone");
	}
}