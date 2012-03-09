package flash.errors;

extern class ScriptTimeoutError extends Error {
	function new(?message : String, id : Int = 0) : Void;
}
