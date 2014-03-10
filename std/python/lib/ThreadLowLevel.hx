
package python.lib;

import python.lib.Types.Tuple;

private typedef TODO = Dynamic;

extern class ThreadLowLevel {
	
	public static function start_new_thread(f:Void->Void, args:Tuple<Dynamic>):TODO;

	static function __init__ ():Void 
	{
		python.Macros.importAs("_thread", "python.lib.ThreadLowLevel");
	}

}