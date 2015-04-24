package flash.net.drm;

extern class DRMURLDownloadContext extends flash.events.EventDispatcher {
	function new() : Void;
	function httpGetASync(url : String) : Void;
	function httpPostAndReceiveASync(url : String, headerName : String, headerValue : String, data : flash.utils.ByteArray, serverType : UInt) : Void;
}
