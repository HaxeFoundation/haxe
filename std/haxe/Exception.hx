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

	`haxe.Exception` is also a wildcard type to catch any exception:
	```haxe
	try {
		throw 'Catch me!';
	} catch(e:haxe.Exception) {
		trace(e.message); // Output: Catch me!
	}
	```

	To rethrow an exception just throw it again.
	Haxe will try to rethrow an original native exception whenever possible.
	```haxe
	try {
		var a:Array<Int> = null;
		a.push(1); // generates target-specific null-pointer exception
	} catch(e:haxe.Exception) {
		throw e; // rethrows native exception instead of haxe.Exception
	}
	```
**/
extern class Exception {
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
		Used internally for wildcard catches like `catch(e:Exception)`.
	**/
	static private function caught(value:Any):Exception;

	/**
		Used internally for wrapping non-throwable values for `throw` expressions.
	**/
	static private function thrown(value:Any):Any;

	/**
		Create a new Exception instance.

		The `previous` argument could be used for exception chaining.

		The `native` argument is for internal usage only.
		There is no need to provide `native` argument manually and no need to keep it
		upon extending `haxe.Exception` unless you know what you're doing.
	**/
	public function new(message:String, ?previous:Exception, ?native:Any):Void;

	/**
		Extract an originally thrown value.

		Used internally for catching non-native exceptions.
		Do _not_ override unless you know what you are doing.
	**/
	private function unwrap():Any;

	/**
		Returns exception message.
	**/
	public function toString():String;

	/**
		Detailed exception description.

		Includes message, stack and the chain of previous exceptions (if set).
	**/
	public function details():String;

	/**
		If this field is defined in a target implementation, then a call to this
		field will be generated automatically in every constructor of derived classes
		to make exception stacks point to derived constructor invocations instead of
		`super` calls.
	**/
	// @:noCompletion @:ifFeature("haxe.Exception.stack") private function __shiftStack():Void;
}
