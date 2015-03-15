package flash.system;

@:final extern class ConnexionsClient {
	function new() : Void;
	function Connexions() : Dynamic;
	function _init(topLocation : String, documentReferrer : String, windowLocation : String, movie : String, userAgent : String, timeout : UInt) : Void;
	function autoAdd(port : Int) : Dynamic;
	function manualAdd(port : Int) : Dynamic;
}
