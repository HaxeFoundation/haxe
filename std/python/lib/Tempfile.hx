
package python.lib;

@:pythonImport("tempfile")
extern class Tempfile {

	public static function gettempdir():String;

}