
package python.lib;

extern class PPrint {

	
	public static function pprint (x:Dynamic):Void;

	public static function pformat(object:Dynamic, indent:Int=1, width:Int=80, depth:Int=null):String;

	static function __init__ ():Void {
		python.Macros.importAs("pprint", "python.lib.PPrint");
	}

}