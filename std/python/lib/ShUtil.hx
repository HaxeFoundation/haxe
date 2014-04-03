
package python.lib;

import python.lib.Types;


extern class ShUtil {

	public static function rmtree(path:String, ?ignore_errors:Bool=false, ?onerror:BaseException->Void):Void;

	public static function copyfile (src:String, dst:String):Void;

	public static function copy (src:String, dst:String):Void;
	public static function copy2 (src:String, dst:String):Void;

	static function __init__ ():Void {
		Syntax.importAs("shutil", "python.lib.ShUtil");
	}
}