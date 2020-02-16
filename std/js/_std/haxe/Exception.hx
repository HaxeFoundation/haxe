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

	static public function caught(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, Error)) {
			return new Exception((cast value:Error).message, null, value);
		} else {
			return new ValueException(value, null, value);
		}
	}

	static public function thrown(value:Any):Any {
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

		this.__nativeException = native != null ? native : this;
		if(Std.isOfType(native, Error) ) {
			(cast this).stack = (cast native).stack;
		} else {
			var stack:String = if ((cast Error).captureStackTrace) {
				(cast Error).captureStackTrace(this, Exception);
				(cast this).stack;
			} else {
				new Error().stack;
			}
			//remove the first line, which is a call to `new Error()`
			(cast this).stack = switch stack.indexOf('\n') {
				case p if(p >= 0): stack.substr(p + 1);
				case _: stack;
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
				__errorStack = CallStack.getStack(cast this);
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