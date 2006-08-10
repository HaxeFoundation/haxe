package flash.system;

extern class Security {
	function new() : Void;
	static var LOCAL_TRUSTED : String;
	static var LOCAL_WITH_FILE : String;
	static var LOCAL_WITH_NETWORK : String;
	static var REMOTE : String;
	static function allowDomain( /* ...arguments */) : Void;
	static function allowInsecureDomain( /* ...arguments */) : Void;
	static var disableAVM1Loading : Bool;
	static var exactSettings : Bool;
	static function loadPolicyFile(url : String) : Void;
	static var sandboxType(default,null) : String;
	static function showSettings(?panel : String) : Void;
}
