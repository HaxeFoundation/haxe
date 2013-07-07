package flash.system;

extern class Security {
	@:require(flash10_1) static var APPLICATION : String;
	static var LOCAL_TRUSTED : String;
	static var LOCAL_WITH_FILE : String;
	static var LOCAL_WITH_NETWORK : String;
	static var REMOTE : String;
	static var disableAVM1Loading : Bool;
	static var exactSettings : Bool;
	@:require(flash11) static var pageDomain(default,null) : String;
	static var sandboxType(default,null) : String;
	static function allowDomain(?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	static function allowInsecureDomain(?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	@:require(flash10_1) static function duplicateSandboxBridgeInputArguments(toplevel : Dynamic, args : Array<Dynamic>) : Array<Dynamic>;
	@:require(flash10_1) static function duplicateSandboxBridgeOutputArgument(toplevel : Dynamic, arg : Dynamic) : Dynamic;
	static function loadPolicyFile(url : String) : Void;
	static function showSettings(?panel : SecurityPanel) : Void;
}
