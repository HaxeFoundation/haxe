package haxe;

/**
	An alias of the base class or interface for native exceptions.
**/
typedef NativeException = Any;

/**
	Base class for exceptions.
**/
extern class Error {
	/**
		Error message.
	**/
	public var message(get,never):String;
	private function get_error():String;

	/**
		The call stack at the moment of the error creation.
	**/
	public var stack(get,never):ErrorStack;
	private function get_stack():ErrorStack;

	/**
		Contains an error, which was passed to `previous` constructor argument.
	**/
	public var previous(get,never):Null<Error>;
	private function get_previous():Null<Error>;

	/**
		Native exception, which caused this error.
	**/
	public var native(get,never):NativeException;
	private function get_native():NativeException;

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
		Error description.

		Includes message, stack and the previous error (if set).
	**/
	public function toString():String;
}
