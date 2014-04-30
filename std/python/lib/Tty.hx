
package python.lib;

@:import("tty", ignoreError=true)
extern class Tty {
	public static function setraw (fileNo:Int):Void;
}