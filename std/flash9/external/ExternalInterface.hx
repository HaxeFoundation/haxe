package flash.external;

extern class ExternalInterface {
	function new() : Void;
	static function addCallback(functionName : String, closure : Dynamic ) : Void;
	static var available(default,null) : Bool;
	static function call(functionName : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Dynamic;
	static var objectID(default,null) : String;

	/** added in FP 9.0.115 **/
	static var marshallExceptions : Bool;

	private static function _addCallback(functionName : String, closure : Dynamic) : Void;
	private static function _argumentsToAS(obj : Dynamic) : Array<Dynamic>;
	private static function _argumentsToXML(obj : Array<Dynamic>) : String;
	private static function _arrayToAS(obj : Dynamic) : Void;
	private static function _arrayToJS(value : Array<Dynamic>) : String;
	private static function _arrayToXML(obj : Array<Dynamic>) : String;
	private static function _callIn(closure : Dynamic, request : String) : String;
	private static function _callOut(request : String) : String;
	private static function _escapeXML(s : String) : String;
	private static function _evalJS(expression : String) : String;
	private static function _getPropNames(obj : Dynamic) : Array<Dynamic>;
	private static function _initJS() : Void;
	private static function _objectToAS(obj : Dynamic) : Void;
	private static function _objectToJS(value : Dynamic) : String;
	private static function _objectToXML(obj : Dynamic) : String;
	private static function _toAS(obj : Dynamic) : Void;
	private static function _toJS(value : Dynamic) : String;
	private static function _toXML(value : Dynamic) : String;
	private static var activeX(default,null) : Bool;
}
