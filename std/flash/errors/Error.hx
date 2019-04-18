package flash.errors;

extern class Error #if !flash_strict implements Dynamic #end {
	var errorID(default,never) : Int;
	var message : Dynamic;
	var name : Dynamic;
	function new(?message : Dynamic, id : Dynamic = 0) : Void;
	function getStackTrace() : String;
	static final length : Int;
	static function getErrorMessage(index : Int) : String;
	static function throwError(type : Class<Dynamic>, index : UInt, restArgs : haxe.extern.Rest<Dynamic>) : Dynamic;
}
