package flash.errors;

extern class Error #if !flash_strict implements Dynamic #end {
	@:flash.property var errorID(get,never) : Int;
	var message : Dynamic;
	var name : Dynamic;
	function new(?message : Dynamic, id : Dynamic = 0) : Void;
	function getStackTrace() : String;
	private function get_errorID() : Int;
	static final length : Int;
	static function getErrorMessage(index : Int) : String;
	static function throwError(type : Class<Dynamic>, index : UInt, restArgs : haxe.extern.Rest<Dynamic>) : Dynamic;
}
