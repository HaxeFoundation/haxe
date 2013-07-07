package flash.display;

extern class LoaderInfo extends flash.events.EventDispatcher {
	var actionScriptVersion(default,null) : ActionScriptVersion;
	var applicationDomain(default,null) : flash.system.ApplicationDomain;
	var bytes(default,null) : flash.utils.ByteArray;
	var bytesLoaded(default,null) : UInt;
	var bytesTotal(default,null) : UInt;
	var childAllowsParent(default,null) : Bool;
	@:require(flash11_4) var childSandboxBridge : Dynamic;
	var content(default,null) : DisplayObject;
	var contentType(default,null) : String;
	var frameRate(default,null) : Float;
	var height(default,null) : Int;
	@:require(flash10_1) var isURLInaccessible(default,null) : Bool;
	var loader(default,null) : Loader;
	var loaderURL(default,null) : String;
	var parameters(default,null) : Dynamic<String>;
	var parentAllowsChild(default,null) : Bool;
	@:require(flash11_4) var parentSandboxBridge : Dynamic;
	var sameDomain(default,null) : Bool;
	var sharedEvents(default,null) : flash.events.EventDispatcher;
	var swfVersion(default,null) : UInt;
	@:require(flash10_1) var uncaughtErrorEvents(default,null) : flash.events.UncaughtErrorEvents;
	var url(default,null) : String;
	var width(default,null) : Int;
	static function getLoaderInfoByDefinition(object : Dynamic) : LoaderInfo;
}
