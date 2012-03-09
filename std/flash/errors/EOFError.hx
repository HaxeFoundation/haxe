package flash.errors;

extern class EOFError extends IOError {
	function new(?message : String, id : Int = 0) : Void;
}
