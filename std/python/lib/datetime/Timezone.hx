
package python.lib.datetime;

@:import("datetime", "timezone")
extern class Timezone extends TzInfo {

	public static var utc(default, never):TzInfo;
}