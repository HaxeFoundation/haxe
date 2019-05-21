package flash.external;

extern class ExternalInterface {
	@:flash.property static var available(get,never) : Bool;
	static var marshallExceptions : Bool;
	@:flash.property static var objectID(get,never) : String;
	static function addCallback(functionName : String, closure : Dynamic) : Void;
	static function call(functionName : String, restArgs : haxe.extern.Rest<Dynamic>) : Dynamic;
	private static function get_available() : Bool;
	private static function get_objectID() : String;
}
