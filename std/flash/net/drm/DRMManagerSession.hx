package flash.net.drm;

extern class DRMManagerSession extends flash.events.EventDispatcher {
	var m_isInSession : Bool;
	var metadata : DRMContentData;
	function new() : Void;
	function checkStatus() : UInt;
	function errorCodeToThrow(errorCode : UInt) : Void;
	function getLastError() : UInt;
	function getLastServerErrorString() : String;
	function getLastSubErrorID() : UInt;
	function issueDRMErrorEvent(metadata : DRMContentData, errorID : Int, subErrorID : Int, serverErrorString : String) : Void;
	function issueDRMStatusEvent(inMetadata : DRMContentData, voucher : DRMVoucher) : Dynamic;
	function onSessionComplete() : Void;
	function onSessionError() : Void;
	function setTimerUp() : Void;
	static var STATUS_FAILED(default,never) : UInt;
	static var STATUS_NOTREADY(default,never) : UInt;
	static var STATUS_READY(default,never) : UInt;
	static var STATUS_UNKNOWN(default,never) : UInt;
}
