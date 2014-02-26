package flash.system;

@:final extern class ConnexionsClient {
	var enabled(default,null) : Bool;
	function new() : Void;
	function Connexions() : Dynamic;
	function _init(port : Int, secret : String, topLocation : String, documentReferrer : String, windowLocation : String, movie : String, userAgent : String) : Void;
	function autoAdd() : Dynamic;
	function manualAdd() : Dynamic;
}
