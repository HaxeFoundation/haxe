package flash.errors;

@:require(flash10_1) extern class DRMManagerError extends Error {
	@:flash.property var subErrorID(get,never) : Int;
	function new(message : String, id : Int, subErrorID : Int) : Void;
	private function get_subErrorID() : Int;
	function toString() : String;
}
