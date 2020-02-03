package haxe;

/**
	An error containing arbitrary value.

	This class is automatically used for throwing values, which don't extend `haxe.Error`
	or native exception type.
	For example:
	```haxe
	throw "Terrible error";
	```
	will be compiled to
	```haxe
	throw new ValueError("Terrible error");
	```
**/
extern class ValueError extends Error {
	/**
		Thrown value.
	**/
	public var value(default,null):Any;

	public function new(value:Any, ?previous:Error):Void;
}


/**
	Base class for exceptions.
	This is a wildcard type to catch any exception.

	TODO: move to the root package for convenience?
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
	public var native(get,never):Any;
	final private function get_native():Any;

	/**
		Get an instance of `haxe.Error` for an arbitrary value.

		Returns the `value` as is, if it's already an instance of `haxe.Error`.

		Used internally for wildcard catches like `catch(e:Error)`.
	**/
	static public function wrap(value:Any):Error;

	/**
		Wrap `value` into a native exception, which can be used for throwing.

		Used internally for wrapping non-throwable values for `throw` expressions.
	**/
	static public function wrapNative(value:Any):Any;

	/**
		Create a new Error instance.

		Upon extending `haxe.Error` for custom error classes there is no need to
		keep `?native:Any` argument unless you know what you're doing.
	**/
	public function new(message:String, ?previous:Error, ?native:Any):Void;

	/**
		Extract an originally thrown value.

		This method must return the same value on subsequent calls.
		Used internally for catching non-native exceptions.
		Do _not_ override unless you know what you are doing.
	**/
	public function unwrap():Any;

	/**
		Error description.

		Includes message, stack and the previous error (if set).
	**/
	public function toString():String;
}
