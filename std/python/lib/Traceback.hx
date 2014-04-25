package python.lib;

import python.lib.Sys;
import python.lib.Sys.Frame;
import python.lib.Types;
import python.lib.Tuple;


extern class Traceback {

	public static function extract_stack(?f:Frame, ?limit:Int):Array<StackItem>;
	public static function extract_tb(tb:Sys.TB, ?limit:Int):Array<StackItem>;

	static function __init__ ():Void {
		python.Syntax.importAs("traceback", "python.lib.Traceback");
	}
}

private typedef StackItem = Tup4<String, Int, String, String>;
