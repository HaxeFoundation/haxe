
package python.lib;

@:pythonImport("pprint")
extern class Pprint {


	public static function pprint (x:Dynamic):Void;

	public static function pformat(object:Dynamic, indent:Int=1, width:Int=80, depth:Int=null):String;

}