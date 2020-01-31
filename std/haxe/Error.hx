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
		Get an instance of `haxe.Error` for a native exception.

		Used internally for wildcard catches like `catch(e:Error)`.
	**/
	static public function ofNative(exception:NativeException):Error;

	/**
		Get an instance of `haxe.Error` for an arbitrary value.

		Used internally for throwing dynamically typed values.
	**/
	static public function ofAny(value:Any):Error;

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
