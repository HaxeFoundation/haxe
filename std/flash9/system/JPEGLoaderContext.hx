package flash.system;

extern class JPEGLoaderContext extends LoaderContext {

	var deblockingFilter : Float;

	function new( ?deblockingFilter : Float, ?checkPolicyFile : Bool, ?applicationDomain:ApplicationDomain, ?securityDomain:SecurityDomain ) : Void;

}
