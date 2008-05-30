package flash.external;

extern class ExternalInterface {
	static var available(default,null) : Bool;

	static var objectID(default,null) : String;
	static function addCallback(functionName : String, closure : Dynamic) : Void;
	static function call(functionName : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Dynamic;
	/** added in FP 9.0.115 **/
	static var marshallExceptions : Bool;
}
