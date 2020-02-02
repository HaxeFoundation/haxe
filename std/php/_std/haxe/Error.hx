package haxe;

import haxe.ErrorStack;
import php.Boot;
import php.Throwable;

typedef NativeException = Throwable;

class ValueError extends Error {
	public var value(default,null):Any;

	public function new(value:Any, ?previous:Error):Void {
		super(Std.string(value), previous);
		this.value = value;
	}

	override public function unwrap():Any {
		return value;
	}
}

@:coreApi
class Error {
	public var message(get,never):String;
	public var stack(get,never):ErrorStack;
	public var previous(get,never):Null<Error>;
	public var native(get,never):NativeException;

	@:noCompletion var __errorMessage:String;
	@:noCompletion var __errorStack:Null<ErrorStack>;
	@:noCompletion var __nativeException:Throwable;
	@:noCompletion var __previousError:Null<Error>;
	@:noCompletion var __isWrapper:Bool;

	static public function ofNative(exception:NativeException):Error {
		if(Boot.isHxException(exception)) {
			return Boot.castHxException(exception).e;
		} else {
			return new Error(exception.getMessage(), null, exception);
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

	public function new(message:String, ?previous:Error, ?native:NativeException) {
		this.__errorMessage = message;
		this.__previousError = previous;
		if(native == null) {
			this.__isWrapper = false;
			this.__nativeException = Boot.createHxException(this);
		} else {
			this.__isWrapper = true;
			this.__nativeException = native;
		}
	}

	public function unwrap():Any {
		return __isWrapper ? native : this;
	}

	public function toString():String {
		if(previous == null) {
			return 'Error: $message\nStack:$stack';
		}
		var result = '';
		var e:Null<Error> = this;
		var prev:Null<Error> = null;
		while(e != null) {
			if(prev == null) {
				result = 'Error: ${e.message}\nStack:${e.stack}' + result;
			} else {
				var prevStack = @:privateAccess e.stack.subtract(prev.stack);
				result = 'Error: ${e.message}\nStack:$prevStack\n\nNext ' + result;
			}
			prev = e;
			e = e.previous;
		}
		return result;
	}

	function get_message():String {
		return __errorMessage;
	}

	function get_previous():Null<Error> {
		return __previousError;
	}

	function get_native():NativeException {
		return __nativeException;
	}

	function get_stack():ErrorStack {
		return switch __errorStack {
			case null:
				__errorStack = @:privateAccess CallStack.makeStack(__nativeException.getTrace());
			case s: s;
		}
	}
}