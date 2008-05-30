package flash;

extern class Error {
	var errorID(default,null) : Int;
	var message : Dynamic;
	var name : Dynamic;
	function new(?message : String, ?id : Int) : Void;
	function getStackTrace() : String;
	static var length : Int;
	static function getErrorMessage(index : Int) : String;
	static function throwError(type : Class<Dynamic>, index : UInt, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Dynamic;
}
