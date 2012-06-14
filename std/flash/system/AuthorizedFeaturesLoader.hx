package flash.system;

@:final @:require(flash11_3) extern class AuthorizedFeaturesLoader extends flash.events.EventDispatcher {
	var authorizedFeatures(default,null) : AuthorizedFeatures;
	function new() : Void;
	function loadAuthorizedFeatures() : Void;
}
