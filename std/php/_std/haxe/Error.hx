package haxe;

import haxe.ErrorStack;
import php.Boot;
import php.Throwable;

typedef NativeException = Throwable;

@:coreApi
class Error {
	public var message(get,never):String;
	public var stack(get,never):ErrorStack;
	public var previous(get,never):Null<Error>;
	public var native(get,never):NativeException;

	@:noCompletion var _errorMessage:String;
	@:noCompletion var _errorStack:Null<ErrorStack>;
	@:noCompletion var _nativeException:Throwable;
	@:noCompletion var _previousError:Null<Error>;

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
		this._errorMessage = message;
		this._previousError = previous;
		this._nativeException = (native == null ? Boot.createHxException(this) : native);
	}

	function get_message():String {
		return _errorMessage;
	}

	function get_previous():Null<Error> {
		return _previousError;
	}

	function get_native():NativeException {
		return _nativeException;
	}

	function get_stack():ErrorStack {
		return switch _errorStack {
			case null:
				_errorStack = @:privateAccess CallStack.makeStack(_nativeException.getTrace());
			case s: s;
		}
	}

	public function toString():String {
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
}