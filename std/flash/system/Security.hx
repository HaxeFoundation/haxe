package flash.system;

extern class Security {
	@:require(flash10_1) static final APPLICATION : String;
	static final LOCAL_TRUSTED : String;
	static final LOCAL_WITH_FILE : String;
	static final LOCAL_WITH_NETWORK : String;
	static final REMOTE : String;
	static var disableAVM1Loading : Bool;
	static var exactSettings : Bool;
	@:require(flash11) static var pageDomain(default,never) : String;
	static var sandboxType(default,never) : String;
	static function allowDomain(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	static function allowInsecureDomain(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	@:require(flash10_1) static function duplicateSandboxBridgeInputArguments(toplevel : Dynamic, args : Array<Dynamic>) : Array<Dynamic>;
	@:require(flash10_1) static function duplicateSandboxBridgeOutputArgument(toplevel : Dynamic, arg : Dynamic) : Dynamic;
	static function loadPolicyFile(url : String) : Void;
	static function showSettings(?panel : SecurityPanel) : Void;
}
