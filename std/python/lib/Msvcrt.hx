
package python.lib;

@:import("msvcrt", ignoreError=true)
extern class Msvcrt {

	public static function getch ():python.lib.Bytes;

}