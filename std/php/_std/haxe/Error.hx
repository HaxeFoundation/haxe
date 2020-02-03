package haxe;

import haxe.ErrorStack;
import php.Boot;
import php.Global;
import php.Throwable;
import php.NativeAssocArray;
import php.NativeIndexedArray;

@:dox(hide)
@:noCompletion
typedef NativeException = MetaThrowable;

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
class Error extends PhpException {
	public var message(get,never):String;
	public var stack(get,never):ErrorStack;
	public var previous(get,never):Null<Error>;
	public var native(get,never):Any;

	@:noCompletion var __errorStack:Null<ErrorStack>;
	@:noCompletion var __nativeException:Throwable;
	@:noCompletion var __previousError:Null<Error>;

	static public function wrap(value:Any):Error {
		if(Std.is(value, Error)) {
			return value;
		} else if(Std.isOfType(value, Throwable)) {
			return new Error((value:Throwable).getMessage(), null, value);
		} else {
			return new ValueError(value);
		}
	}

	public function new(message:String, ?previous:Error, ?native:Any) {
		super(message, 0, previous);
		this.__previousError = previous;
		this.__nativeException = native == null ? cast this : native;
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
		return this.getMessage();
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
@:native('Exception')
private extern class PhpException implements MetaThrowable {
	@:noCompletion private function new(?message:String, ?code:Int, ?previous:MetaThrowable):Void;

	@:noCompletion private var code:Int;
	@:noCompletion private var file:String;
	@:noCompletion private var line:Int;

	@:noCompletion final private function getPrevious():Throwable;
	@:noCompletion private function getMessage():String;
	@:noCompletion private function getCode():Int;
	@:noCompletion private function getFile():String;
	@:noCompletion private function getLine():Int;
	@:noCompletion private function getTrace():NativeIndexedArray<NativeAssocArray<Dynamic>>;
	@:noCompletion private function getTraceAsString():String;
	@:noCompletion @:phpMagic private function __toString():String;
}