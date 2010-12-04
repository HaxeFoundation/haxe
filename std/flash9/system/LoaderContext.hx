package flash.system;

extern class LoaderContext {
	var applicationDomain : ApplicationDomain;
	var checkPolicyFile : Bool;
	var securityDomain : SecurityDomain;
	function new(checkPolicyFile : Bool = false, ?applicationDomain : ApplicationDomain, ?securityDomain : SecurityDomain) : Void;
}
