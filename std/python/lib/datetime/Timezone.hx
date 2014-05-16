
package python.lib.datetime;

@:pythonImport("datetime", "timezone")
extern class Timezone extends TzInfo {

	public static var utc(default, never):TzInfo;
}