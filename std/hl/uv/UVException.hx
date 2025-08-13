package hl.uv;

import haxe.Exception;

/**
	Exceptions thrown by functions in `hl.uv` package.
**/
class UVException extends Exception {
	/**
		The error.
	**/
	public final error:UVError;

	/**
		Instantiates an error with given message and position.
	**/
	#if hl @:keep #end
	public function new(error:UVError, ?message:String, ?previous:Exception) {
		if(message == null)
			message = error.toString();
		super(message, previous);
		this.error = error;
	}
}