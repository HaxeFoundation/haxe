package flash.system;

extern class Security
{
	static function allowDomain( domain : String ):Void;
	static function allowInsecureDomain( domain : String ):Void;
	static function loadPolicyFile(url:String):Void;

	#if flash8
	static function sandboxType(default,null) : String;
	#end

	private static function __init__() : Void untyped {
		flash.system.Security = _global["System"]["security"];
	}

}
