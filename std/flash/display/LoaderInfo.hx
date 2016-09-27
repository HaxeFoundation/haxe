package flash.display;

extern class LoaderInfo extends flash.events.EventDispatcher {
	var actionScriptVersion(default,never) : ActionScriptVersion;
	var applicationDomain(default,never) : flash.system.ApplicationDomain;
	var bytes(default,never) : flash.utils.ByteArray;
	var bytesLoaded(default,never) : UInt;
	var bytesTotal(default,never) : UInt;
	var childAllowsParent(default,never) : Bool;
	@:require(flash11_4) var childSandboxBridge : Dynamic;
	var content(default,never) : DisplayObject;
	var contentType(default,never) : String;
	var frameRate(default,never) : Float;
	var height(default,never) : Int;
	@:require(flash10_1) var isURLInaccessible(default,never) : Bool;
	var loader(default,never) : Loader;
	var loaderURL(default,never) : String;
	var parameters(default,never) : Dynamic<String>;
	var parentAllowsChild(default,never) : Bool;
	@:require(flash11_4) var parentSandboxBridge : Dynamic;
	var sameDomain(default,never) : Bool;
	var sharedEvents(default,never) : flash.events.EventDispatcher;
	var swfVersion(default,never) : UInt;
	@:require(flash10_1) var uncaughtErrorEvents(default,never) : flash.events.UncaughtErrorEvents;
	var url(default,never) : String;
	var width(default,never) : Int;
	static function getLoaderInfoByDefinition(object : Dynamic) : LoaderInfo;
}
