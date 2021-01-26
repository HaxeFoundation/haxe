package eval.luv;

/**
	Exceptions thrown by functions in `eval.luv` package.
**/
class LuvException extends haxe.Exception {
	/**
		The error.
	**/
	public final error:UVError;

	/**
		Instantiates an error with given message and position.
	**/
	public function new(error:UVError, ?message:String, ?previous:haxe.Exception) {
		super(message == null ? error.toString() : message, previous);
		this.error = error;
	}
}