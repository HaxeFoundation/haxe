package flash.external;

extern class ExternalInterface {
	static var available(default,never) : Bool;
	static var marshallExceptions : Bool;
	static var objectID(default,never) : String;
	static function addCallback(functionName : String, closure : Dynamic) : Void;
	static function call(functionName : String, restArgs : haxe.extern.Rest<Dynamic>) : Dynamic;
}
