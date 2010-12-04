package flash.system;

@:require(flash10) extern class JPEGLoaderContext extends LoaderContext {
	var deblockingFilter : Float;
	function new(deblockingFilter : Float = 0, checkPolicyFile : Bool = false, ?applicationDomain : ApplicationDomain, ?securityDomain : SecurityDomain) : Void;
}
