
package python.lib;

import python.lib.Types.BaseException;

extern class ShUtil {

	public static function rmtree(path:String, ?ignore_errors:Bool=false, ?onerror:BaseException->Void):Void;

	static function __init__ ():Void {
		Macros.importAs("shutil", "python.lib.ShUtil");
	}
}