package python.lib;

import python.lib.Sys;
import python.lib.Sys.Frame;
import python.lib.Tuple;

@:pythonImport("traceback")
extern class Traceback {

	public static function extract_stack(?f:Frame, ?limit:Int):Array<StackItem>;
	public static function extract_tb(tb:Sys.TB, ?limit:Int):Array<StackItem>;

}

private typedef StackItem = Tup4<String, Int, String, String>;
