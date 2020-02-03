package haxe;

import haxe.ErrorStack;
import js.Syntax;

@:dox(hide)
@:noCompletion
typedef NativeException = Dynamic;

class ValueError extends Error {
	public var value(default,null):Any;

	public function new(value:Any, ?previous:Error):Void {
		super(inline Std.string(value), previous, this);
		this.value = value;
	}

	override public function unwrap():Any {
		return value;
	}
}

@:coreApi
class Error extends JsError {
	public var message(get,never):String;
	public var stack(get,never):ErrorStack;
	public var previous(get,never):Null<Error>;
	public var native(get,never):Any;

	@:noCompletion var __errorStack:Null<ErrorStack>;
	@:noCompletion var __nativeException:Any;
	@:noCompletion var __previousError:Null<Error>;

	static public function wrap(value:Any):Error {
		if(Syntax.instanceof(value, Error)) {
			return value;
		} else if(Syntax.instanceof(value, js.lib.Error)) {
			return new Error((cast value).message, null, value);
		} else {
			return new ValueError(value);
		}
	}

	public function new(message:String, ?previous:Error, ?native:Any) {
		super(message);
		(cast this:js.lib.Error).message = message;
		this.__previousError = previous;

		if(native != null) {
			this.__nativeException = native;
			(cast this).stack = Syntax.instanceof(native, js.lib.Error) ? (cast native).stack : null;
		} else {
			this.__nativeException = this;
			if ((cast js.lib.Error).captureStackTrace) {
				(cast js.lib.Error).captureStackTrace(this, Error);
			} else {
				var stack:String = new js.lib.Error().stack;
				//remove the first line, which is a call to `new js.lib.Error()`
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
		return (cast this:js.lib.Error).message;
	}

	function get_previous():Null<Error> {
		return __previousError;
	}

	function get_native():Any {
		return __nativeException;
	}

	function get_stack():ErrorStack {
		return switch __errorStack {
			case null:
				__errorStack = CallStack.makeStack((cast this:js.lib.Error).stack);
				__errorStack = CallStack.getStack(cast this);
			case s: s;
		}
	}
}

@:dox(hide)
@:noCompletion
@:native('Error')
private extern class JsError {
	// private var message:String; //redefined in haxe.Error
	@:noCompletion private var name:String;
	// private var stack(default, null):String; //redefined in haxe.Error

	private function new(?message:String):Void;
}