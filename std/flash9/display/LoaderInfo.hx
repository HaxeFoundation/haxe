package flash.display;

extern class LoaderInfo extends flash.events.EventDispatcher {
	function new() : Void;
	var actionScriptVersion(default,null) : UInt;
	var applicationDomain(default,null) : flash.system.ApplicationDomain;
	var bytesLoaded(default,null) : UInt;
	var bytesTotal(default,null) : UInt;
	var childAllowsParent(default,null) : Bool;
	var content(default,null) : flash.display.DisplayObject;
	var contentType(default,null) : String;
	var frameRate(default,null) : Float;
	var height(default,null) : Int;
	var loader(default,null) : flash.display.Loader;
	var loaderURL(default,null) : String;
	var parameters(default,null) : Dynamic;
	var parentAllowsChild(default,null) : Bool;
	var sameDomain(default,null) : Bool;
	var sharedEvents(default,null) : flash.events.EventDispatcher;
	var swfVersion(default,null) : UInt;
	var url(default,null) : String;
	var width(default,null) : Int;
	private function _getArgs() : Dynamic;
}
