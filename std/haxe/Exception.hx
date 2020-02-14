package haxe;

/**
	Base class for exceptions.

	If this class (or derivatives) is used to catch an exception, then
	`haxe.CallStack.exceptionStack()` will not return a stack for the exception
	caught. Use `haxe.Exception.stack` property instead:
	```haxe
	try {
		throwSomething();
	} catch(e:Exception) {
		trace(e.stack);
	}
	```

	Custom exceptions should extend this class:
	```haxe
	class MyException extends haxe.Exception {}
	//...
	throw new MyException('terrible exception');
	```

	To rethrow `haxe.Exception`-based exceptions just throw them again:
	```haxe
	try {
		throw new MyException();
	} catch(e:MyException) {
		throw e;
	}
	```

	`haxe.Exception` is also a wildcard type to catch any exception:
	```haxe
	try {
		throw 'Catch me!';
	} catch(e:haxe.Exception) {
		trace(e.message); // Output: Catch me!
	}
	```

	To rethrow original native exception use `haxe.Exception.native` property:
	```haxe
	try {
		var a:Array<Int> = null;
		a.push(1); // generates target-specific null-pointer exception
	} catch(e:haxe.Exception) {
		throw e.native; // rethrows native exception instead of haxe.Exception
	}
	```

	TODO: move to the root package for convenience?
**/
extern class Exception extends NativeException {
	/**
		Exception message.
	**/
	public var message(get,never):String;
	private function get_message():String;

	/**
		The call stack at the moment of the exception creation.
	**/
	public var stack(get,never):CallStack;
	private function get_stack():CallStack;

	/**
		Contains an exception, which was passed to `previous` constructor argument.
	**/
	public var previous(get,never):Null<Exception>;
	private function get_previous():Null<Exception>;

	/**
		Native exception, which caused this exception.
	**/
	public var native(get,never):Any;
	final private function get_native():Any;

	/**
		Get an instance of `haxe.Exception` for an arbitrary value.

		Returns the `value` as is, if it's already an instance of `haxe.Exception`.

		Used internally for wildcard catches like `catch(e:Exception)`.
	**/
	static public function wrap(value:Any):Exception;

	/**
		Wrap `value` into a native exception, which can be used for throwing.

		Used internally for wrapping non-throwable values for `throw` expressions.
	**/
	static public function wrapNative(value:Any):Any;

	/**
		Create a new Exception instance.

		Upon extending `haxe.Exception` for custom exception classes there is no need to
		keep `?native:Any` argument unless you know what you're doing.
	**/
	public function new(message:String, ?previous:Exception, ?native:Any):Void;

	/**
		Extract an originally thrown value.

		This method must return the same value on subsequent calls.
		Used internally for catching non-native exceptions.
		Do _not_ override unless you know what you are doing.
	**/
	public function unwrap():Any;

	/**
		Exception description.

		Includes message, stack and the previous exception (if set).
	**/
	public function toString():String;
}

/**
	This class is used to make `haxe.Exception` extend a native exception type
	at runtime without exposing native exception API on `haxe.Exception` instances.
**/
@:dox(hide)
@:noCompletion
@:native('Exception')
private extern class NativeException {}