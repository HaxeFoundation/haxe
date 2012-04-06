package flash.system;

@:require(flash11_3) @:final extern class AuthorizedFeaturesLoader extends flash.events.EventDispatcher {
	var authorizedFeatures(default,null) : AuthorizedFeatures;
	function new() : Void;
	function loadAuthorizedFeatures() : Void;
}
