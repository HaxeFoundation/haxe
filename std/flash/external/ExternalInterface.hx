package flash.external;

extern class ExternalInterface {

	static var available : Bool;
	static function addCallback( methodName : String, instance : Dynamic, method : Dynamic ) : Bool;
	static var call : Dynamic;

}