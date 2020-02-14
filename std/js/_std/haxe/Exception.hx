package haxe;

import js.lib.Error;

@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __errorStack:Null<CallStack>;
	@:noCompletion var __nativeException:Any;
	@:noCompletion var __previousException:Null<Exception>;

	static public function wrap(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, Error)) {
			return new Exception((cast value:Error).message, null, value);
		} else {
			return new ValueException(value);
		}
	}

	static public function wrapNative(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			return (value:Exception).native;
		} else if(Std.isOfType(value, Error)) {
			return value;
		} else {
			return new ValueException(value);
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		super(message);
		(cast this:Error).message = message;
		this.__previousException = previous;

		if(native != null) {
			this.__nativeException = native;
			(cast this).stack = Std.is(native, Error) ? (cast native).stack : null;
		} else {
			this.__nativeException = this;
			if ((cast Error).captureStackTrace) {
				(cast Error).captureStackTrace(this, Exception);
			} else {
				var stack:String = new Error().stack;
				//remove the first line, which is a call to `new Error()`
				(cast this).stack = switch stack.indexOf('\n') {
					case p if(p >= 0): stack.substr(p + 1);
					case _: stack;
				}
			}
		}
	}

	public function unwrap():Any {
		return __nativeException;
	}

	public function toString():String {
		return inline CallStack.exceptionToString(this);
	}

	function get_message():String {
		return (cast this:Error).message;
	}

	function get_previous():Null<Exception> {
		return __previousException;
	}

	final function get_native():Any {
		return __nativeException;
	}

	function get_stack():CallStack {
		return switch __errorStack {
			case null:
				__errorStack = CallStack.getStack(native);
			case s: s;
		}
	}
}

@:dox(hide)
@:noCompletion
@:native('Error')
private extern class NativeException {
	// private var message:String; //redefined in haxe.Exception
	@:noCompletion private var name:String;
	// private var stack(default, null):String; //redefined in haxe.Exception

	private function new(?message:String):Void;
}