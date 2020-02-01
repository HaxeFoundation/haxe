package haxe;

import php.Boot;
import php.Throwable;

typedef NativeException = Throwable;

class Error {
	public var message(default,null):String;

	var stack:Null<ErrorStack>;
	var native:Throwable;

	static public function ofNative(exception:NativeException):Error {
		if(Boot.isHxException(exception)) {
			return Boot.castHxException(exception).e;
		} else {
			return new Error(exception.getMessage(), exception);
		}
	}

	static public function ofAny(value:Any):Error {
		if(Std.isOfType(value, Throwable)) {
			return ofNative(value);
		} else if(Std.isOfType(value, Error)) {
			return value;
		} else {
			return new ValueError(value);
		}
	}

	public function new(message:String, ?native:NativeException) {
		this.message = message;
		this.native = (native == null ? Boot.createHxException(this) : native);
	}

	public function getNative():NativeException {
		return native;
	}

	public function getStack():ErrorStack {
		return switch stack {
			case null:
				stack = @:privateAccess CallStack.makeStack(native.getTrace());
			case stack:
				stack;
		}
	}

	public function toString():String {
		return message;
	}
}
