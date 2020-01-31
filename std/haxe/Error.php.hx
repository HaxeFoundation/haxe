package haxe;

import php.Boot;
import php.Throwable;

typedef NativeException = Throwable;

/**
	Common class for errors.
**/
class Error {
	/**
		Error message.
	**/
	public var message(default,null):String;

	var stack:Null<ErrorStack>;
	var native:Throwable;

	/**
		Wrap native exception into an instance of `haxe.Error`
	**/
	static public function ofNative(exception:NativeException):Error {
		if(Boot.isHxException(exception)) {
			return Boot.castHxException(exception).e;
		} else {
			return new Error(exception.getMessage(), exception);
		}
	}

	public function new(message:String, ?native:NativeException) {
		this.message = message;
		this.native = (native == null ? Boot.createHxException(this) : native);
	}

	/**
		Retrieve native exception, representing this error.
	**/
	public function getNative():NativeException {
		return native;
	}

	/**
		Error call stack.
	**/
	public function getStack():ErrorStack {
		return switch stack {
			case null:
				stack = @:privateAccess CallStack.makeStack(native.getTrace());
			case stack:
				stack;
		}
	}

	/**
		Error description.
	**/
	public function toString():String {
		return 'Error: $message\n${getStack()}';
	}
}
