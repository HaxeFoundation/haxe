package flash.system;

extern class Security
{
	static function allowDomain( domain : String ):Void;
	static function allowInsecureDomain( domain : String ):Void;
	static function loadPolicyFile(url:String):Void;

	private static function __init__() : Void untyped {
		flash.system.Security = _global["System"]["security"];
	}

}
