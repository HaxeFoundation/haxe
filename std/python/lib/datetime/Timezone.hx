package python.lib.datetime;

@:pythonImport("datetime", "timezone")
extern class Timezone extends Tzinfo {

	public static var utc(default, never):Tzinfo;
}