package flash.system;

@:require(flash11_3) extern final class AuthorizedFeaturesLoader extends flash.events.EventDispatcher {
	var authorizedFeatures(get,never) : AuthorizedFeatures;
	function new() : Void;
	private function get_authorizedFeatures() : AuthorizedFeatures;
	function loadAuthorizedFeatures() : Void;
	@:require(flash11_7) function loadAuthorizedFeaturesFromData(data : flash.utils.ByteArray) : Void;
	@:require(flash11_4) function makeGlobal() : Void;
}
