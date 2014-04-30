
package python.lib;

import python.lib.Tuple;

private typedef TODO = Dynamic;

@:import("_thread")
extern class ThreadLowLevel {

	public static function start_new_thread(f:Void->Void, args:Tuple<Dynamic>):TODO;

}