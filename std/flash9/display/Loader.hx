package flash.display;

extern class Loader extends flash.display.DisplayObjectContainer {
	function new() : Void;
	function close() : Void;
	var content(default,null) : flash.display.DisplayObject;
	var contentLoaderInfo(default,null) : flash.display.LoaderInfo;
	function load(request : flash.net.URLRequest, ?context : flash.system.LoaderContext) : Void;
	function loadBytes(bytes : flash.utils.ByteArray, ?context : flash.system.LoaderContext) : Void;
	function unload() : Void;
	private function _buildLoaderContext(context : flash.system.LoaderContext) : flash.system.LoaderContext;
	private function _load(request : flash.net.URLRequest, checkPolicyFile : Bool, applicationDomain : flash.system.ApplicationDomain, securityDomain : flash.system.SecurityDomain) : Void;
	private function _loadBytes(bytes : flash.utils.ByteArray, checkPolicyFile : Bool, applicationDomain : flash.system.ApplicationDomain, securityDomain : flash.system.SecurityDomain) : Void;
}
