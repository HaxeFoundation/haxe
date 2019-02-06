package flash.system;

@:require(flash11_3) extern final class AuthorizedFeaturesLoader extends flash.events.EventDispatcher {
	var authorizedFeatures(default,never) : AuthorizedFeatures;
	function new() : Void;
	function loadAuthorizedFeatures() : Void;
	@:require(flash11_7) function loadAuthorizedFeaturesFromData(data : flash.utils.ByteArray) : Void;
	@:require(flash11_4) function makeGlobal() : Void;
}
