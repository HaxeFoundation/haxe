package python.lib;

import python.lib.Types;

extern class Traceback {

	public static function extract_stack(?f:Frame, ?limit:Int):Array<StackItem>;
	public static function extract_tb(tb:TB, ?limit:Int):Array<StackItem>;

	static function __init__ ():Void {
		python.Syntax.importAs("traceback", "python.lib.Traceback");
	}
}

private typedef StackItem = Tup4<String, Int, String, String>;
