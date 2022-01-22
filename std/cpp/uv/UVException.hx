package cpp.uv;

import haxe.Exception;

/**
	Exceptions thrown by functions in `cpp.uv` package.
**/
class UVException extends Exception {
	/**
		The error.
	**/
	public final error:UVError;

	/**
		Instantiates an error with given message and position.
	**/
	public function new(error:UVError, ?message:String, ?previous:Exception) {
		if(message == null)
			message = error.toString();
		super(message, previous);
		this.error = error;
	}
}