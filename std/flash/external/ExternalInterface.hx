package flash.external;

extern class ExternalInterface {
	static var available(default,never) : Bool;
	static var marshallExceptions : Bool;
	static var objectID(default,never) : String;
	static function addCallback(functionName : String, closure : Dynamic) : Void;
	static function call(functionName : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Dynamic;
}
