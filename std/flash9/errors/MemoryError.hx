package flash.errors;

extern class MemoryError extends Error {
	function new(?message : String, id : Int = 0) : Void;
}
