package flash.net.drm;

extern class DRMResetContext extends DRMManagerSession {
	var m_isAutoReset : Bool;
	function new(isAutoReset : Bool) : Void;
	function doReset() : Void;
}
