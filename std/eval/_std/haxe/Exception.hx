package haxe;

@:coreApi
class Exception {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionMessage:String;
	@:noCompletion var __exceptionStack:CallStack;
	@:noCompletion var __nativeException:Any;
	@:noCompletion var __previousException:Null<Exception>;

	static public function caught(value:Any):Exception {
		if(Std.is(value, Exception)) {
			return value;
		} else {
			return new ValueException(value, null, value);
		}
	}

	static public function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			return (value:Exception).native;
		} else {
			var e = new ValueException(value);
			e.__exceptionStack.asArray().shift();
			return e;
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		__exceptionMessage = message;
		__previousException = previous;
		if(native != null) {
			__exceptionStack = CallStack.exceptionStack();
			__nativeException = native;
		} else {
			__exceptionStack = CallStack.getCallStack();
			__nativeException = this;
		}
	}

	public function unwrap():Any {
		return __nativeException;
	}

	public function toString():String {
		return inline CallStack.exceptionToString(this);
	}

	function get_message():String {
		return __exceptionMessage;
	}

	function get_previous():Null<Exception> {
		return __previousException;
	}

	final function get_native():Any {
		return __nativeException;
	}

	function get_stack():CallStack {
		return __exceptionStack;
	}
}