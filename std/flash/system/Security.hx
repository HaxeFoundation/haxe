package flash.system;

extern class Security
{
	static function allowDomain():Void;
	static function allowInsecureDomain():Void;
	static function loadPolicyFile(url:String):Void;

	private static function __init__() : Void untyped {
		flash.system.Security = _global["System"]["security"];
	}

}
