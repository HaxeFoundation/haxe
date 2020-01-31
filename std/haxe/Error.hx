package haxe;

typedef NativeException = Any;

/**
	Common class for errors.
**/
extern class Error {
	/**
		Error message.
	**/
	public var message(default,null):String;

	/**
		Wrap native exception into an instance of `haxe.Error`
	**/
	static public function ofNative(exception:NativeException):Error;

	/**
		Create a new Error instance.

		Upon extending `haxe.Error` for custom error classes there is no need to
		keep `?native:NativeException` argument unless you know what you're doing.
	**/
	public function new(message:String, ?previous:Error, ?native:NativeException):Void;

	/**
		Retrieve native exception, which caused this error.
	**/
	public function getNative():NativeException;

	/**
		Error call stack.
	**/
	public function getStack():ErrorStack;

	/**
		Error description.
	**/
	public function toString():String;
}
