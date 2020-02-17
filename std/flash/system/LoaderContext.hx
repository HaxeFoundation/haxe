package flash.system;

extern class LoaderContext {
	@:require(flash10_1) var allowCodeImport : Bool;
	@:flash.property @:require(flash10_1) var allowLoadBytesCodeExecution(get,set) : Bool;
	var applicationDomain : ApplicationDomain;
	var checkPolicyFile : Bool;
	@:require(flash11) var imageDecodingPolicy : ImageDecodingPolicy;
	@:require(flash11) var parameters : Dynamic;
	@:require(flash11) var requestedContentParent : flash.display.DisplayObjectContainer;
	var securityDomain : SecurityDomain;
	function new(checkPolicyFile : Bool = false, ?applicationDomain : ApplicationDomain, ?securityDomain : SecurityDomain) : Void;
	private function get_allowLoadBytesCodeExecution() : Bool;
	private function set_allowLoadBytesCodeExecution(value : Bool) : Bool;
}
