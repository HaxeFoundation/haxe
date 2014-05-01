
package python.lib;


@:pythonImport("shutil")
extern class ShUtil {

	public static function rmtree(path:String, ?ignore_errors:Bool=false, ?onerror:python.lib.Exceptions.BaseException->Void):Void;

	public static function copyfile (src:String, dst:String):Void;

	public static function copy (src:String, dst:String):Void;
	public static function copy2 (src:String, dst:String):Void;
}