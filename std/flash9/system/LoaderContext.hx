package flash.system;

extern class LoaderContext {
	@:require(flash10_1) var allowCodeImport : Bool;
	@:require(flash10_1) var allowLoadBytesCodeExecution : Bool;
	var applicationDomain : ApplicationDomain;
	var checkPolicyFile : Bool;
	var securityDomain : SecurityDomain;
	function new(checkPolicyFile : Bool = false, ?applicationDomain : ApplicationDomain, ?securityDomain : SecurityDomain) : Void;
}
