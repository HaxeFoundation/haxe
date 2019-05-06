package flash.system;

extern class Security {
	@:require(flash10_1) static final APPLICATION : String;
	static final LOCAL_TRUSTED : String;
	static final LOCAL_WITH_FILE : String;
	static final LOCAL_WITH_NETWORK : String;
	static final REMOTE : String;
	@:flash.property static var disableAVM1Loading(get,set) : Bool;
	@:flash.property static var exactSettings(get,set) : Bool;
	@:flash.property @:require(flash11) static var pageDomain(get,never) : String;
	@:flash.property static var sandboxType(get,never) : String;
	static function allowDomain(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	static function allowInsecureDomain(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	@:ns("flash.system",internal) @:require(flash10_1) static function duplicateSandboxBridgeInputArguments(toplevel : Dynamic, args : Array<Dynamic>) : Array<Dynamic>;
	@:ns("flash.system",internal) @:require(flash10_1) static function duplicateSandboxBridgeOutputArgument(toplevel : Dynamic, arg : Dynamic) : Dynamic;
	private static function get_disableAVM1Loading() : Bool;
	private static function get_exactSettings() : Bool;
	private static function get_pageDomain() : String;
	private static function get_sandboxType() : String;
	static function loadPolicyFile(url : String) : Void;
	private static function set_disableAVM1Loading(value : Bool) : Bool;
	private static function set_exactSettings(value : Bool) : Bool;
	static function showSettings(?panel : SecurityPanel) : Void;
}
