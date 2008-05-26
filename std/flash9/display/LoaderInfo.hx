package flash.display;

extern class LoaderInfo extends flash.events.EventDispatcher {
	var actionScriptVersion(default,null) : ActionScriptVersion;
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
	var parameters(default,null) : Dynamic<String>;
	var parentAllowsChild(default,null) : Bool;
	var sameDomain(default,null) : Bool;
	var sharedEvents(default,null) : flash.events.EventDispatcher;
	var swfVersion(default,null) : SWFVersion;
	var url(default,null) : String;
	var width(default,null) : Int;
	private function _getArgs() : Dynamic;

	/** new in FP 9.0.115 **/
	var bytes(default,null) : flash.utils.ByteArray;

	/** new in FP 9.0.115 **/
	static function getLoaderInfoByDefinition(object : Dynamic) : flash.display.LoaderInfo;

}
