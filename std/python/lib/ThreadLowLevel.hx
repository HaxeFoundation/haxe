
package python.lib;

import python.lib.Tuple;

private typedef TODO = Dynamic;

@:pythonImport("_thread")
extern class ThreadLowLevel {

	public static function start_new_thread(f:Void->Void, args:Tuple<Dynamic>):TODO;

}