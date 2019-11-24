package flash.net.drm;

extern class DRMManagerSession extends flash.events.EventDispatcher {
	var m_isInSession : Bool;
	@:flash.property var metadata(get,set) : DRMContentData;
	function new() : Void;
	function checkStatus() : UInt;
	function errorCodeToThrow(errorCode : UInt) : Void;
	function getLastError() : UInt;
	function getLastServerErrorString() : String;
	function getLastSubErrorID() : UInt;
	private function get_metadata() : DRMContentData;
	function issueDRMErrorEvent(metadata : DRMContentData, errorID : Int, subErrorID : Int, serverErrorString : String) : Void;
	function issueDRMStatusEvent(inMetadata : DRMContentData, voucher : DRMVoucher) : Dynamic;
	function onSessionComplete() : Void;
	function onSessionError() : Void;
	function setTimerUp() : Void;
	private function set_metadata(value : DRMContentData) : DRMContentData;
	@:ns("flash.net.drm",internal) static final STATUS_FAILED : UInt;
	@:ns("flash.net.drm",internal) static final STATUS_NOTREADY : UInt;
	@:ns("flash.net.drm",internal) static final STATUS_READY : UInt;
	@:ns("flash.net.drm",internal) static final STATUS_UNKNOWN : UInt;
}
