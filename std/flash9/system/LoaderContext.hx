package flash.system;

extern class LoaderContext {
	function new(?checkPolicyFile : Bool, ?applicationDomain : flash.system.ApplicationDomain, ?securityDomain : flash.system.SecurityDomain) : Void;
	var applicationDomain : flash.system.ApplicationDomain;
	var checkPolicyFile : Bool;
	var securityDomain : flash.system.SecurityDomain;
}
