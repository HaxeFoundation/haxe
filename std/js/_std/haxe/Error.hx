package haxe;

import haxe.ErrorStack;

@:dox(hide)
@:noCompletion
typedef NativeException = Dynamic;

class ValueError extends Error {
	public var value(default,null):Any;

	public function new(value:Any, ?previous:Error):Void {
		super(inline Std.string(value), previous);
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
	@:noCompletion var __nativeException:Throwable;
	@:noCompletion var __previousError:Null<Error>;

	static function wrap(value:Any):Error {
		if(Std.isOfType(value, Throwable)) {
			return ofNative(value);
		} else {
			return new ValueError(value);
		}
	}

	public function new(message:String, ?previous:Error, ?native:Any) {
		super(message);
		this.__nativeException = native == null ? this : native;

		if ((cast js.lib.Error).captureStackTrace)
			(cast js.lib.Error).captureStackTrace(this, HaxeError);
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
				var nativeTrace = CallStack.complementTrace(__nativeException.getTrace(), __nativeException);
				__errorStack = CallStack.makeStack(nativeTrace);
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