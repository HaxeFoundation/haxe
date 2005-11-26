package flash.system;

extern class Security
{
	static function allowDomain():Void;
	static function allowInsecureDomain():Void;
	static function loadPolicyFile(url:String):Void;
}
