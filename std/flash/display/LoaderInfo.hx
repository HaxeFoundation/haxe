package flash.display;

extern class LoaderInfo extends flash.events.EventDispatcher {
	var actionScriptVersion(get,never) : ActionScriptVersion;
	var applicationDomain(get,never) : flash.system.ApplicationDomain;
	var bytes(get,never) : flash.utils.ByteArray;
	var bytesLoaded(get,never) : UInt;
	var bytesTotal(get,never) : UInt;
	var childAllowsParent(get,never) : Bool;
	@:require(flash11_4) var childSandboxBridge(get,set) : Dynamic;
	var content(get,never) : DisplayObject;
	var contentType(get,never) : String;
	var frameRate(get,never) : Float;
	var height(get,never) : Int;
	@:require(flash10_1) var isURLInaccessible(get,never) : Bool;
	var loader(get,never) : Loader;
	var loaderURL(get,never) : String;
	var parameters(get,never) : Dynamic<String>;
	var parentAllowsChild(get,never) : Bool;
	@:require(flash11_4) var parentSandboxBridge(get,set) : Dynamic;
	var sameDomain(get,never) : Bool;
	var sharedEvents(get,never) : flash.events.EventDispatcher;
	var swfVersion(get,never) : UInt;
	@:require(flash10_1) var uncaughtErrorEvents(get,never) : flash.events.UncaughtErrorEvents;
	var url(get,never) : String;
	var width(get,never) : Int;
	private function get_actionScriptVersion() : ActionScriptVersion;
	private function get_applicationDomain() : flash.system.ApplicationDomain;
	private function get_bytes() : flash.utils.ByteArray;
	private function get_bytesLoaded() : UInt;
	private function get_bytesTotal() : UInt;
	private function get_childAllowsParent() : Bool;
	private function get_childSandboxBridge() : Dynamic;
	private function get_content() : DisplayObject;
	private function get_contentType() : String;
	private function get_frameRate() : Float;
	private function get_height() : Int;
	private function get_isURLInaccessible() : Bool;
	private function get_loader() : Loader;
	private function get_loaderURL() : String;
	private function get_parameters() : Dynamic<String>;
	private function get_parentAllowsChild() : Bool;
	private function get_parentSandboxBridge() : Dynamic;
	private function get_sameDomain() : Bool;
	private function get_sharedEvents() : flash.events.EventDispatcher;
	private function get_swfVersion() : UInt;
	private function get_uncaughtErrorEvents() : flash.events.UncaughtErrorEvents;
	private function get_url() : String;
	private function get_width() : Int;
	private function set_childSandboxBridge(value : Dynamic) : Dynamic;
	private function set_parentSandboxBridge(value : Dynamic) : Dynamic;
	static function getLoaderInfoByDefinition(object : Dynamic) : LoaderInfo;
}
