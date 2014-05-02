
package python.lib.urllib;

@:pythonImport("urllib.parse")
extern class Parse {
	public static function quote (s:String):String;
	public static function unquote (s:String):String;
}