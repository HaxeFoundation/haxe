
package python.lib;

@:pythonImport("tty", ignoreError=true)
extern class Tty {
	public static function setraw (fileNo:Int):Void;
}