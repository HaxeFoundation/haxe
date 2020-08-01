package haxe;

import php.Throwable;
import php.NativeAssocArray;
import php.NativeIndexedArray;

@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeException:Throwable;
	@:noCompletion var __skipStack:Int = 0;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, Throwable)) {
			return new Exception((value:Throwable).getMessage(), null, value);
		} else {
			return new ValueException(value, null, value);
		}
	}

	static function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			return (value:Exception).native;
		} else if(Std.isOfType(value, Throwable)) {
			return value;
		} else {
			var e = new ValueException(value);
			e.__skipStack = 1;
			return e;
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		super(message, 0, previous);
		this.__previousException = previous;
		if(native != null && Std.isOfType(native, Throwable)) {
			__nativeException = native;
		} else {
			__nativeException = cast this;
		}
	}

	function unwrap():Any {
		return __nativeException;
	}

	public function toString():String {
		return message;
	}

	public function details():String {
		return inline CallStack.exceptionToString(this);
	}

	function get_message():String {
		return this.getMessage();
	}

	function get_previous():Null<Exception> {
		return __previousException;
	}

	final function get_native():Any {
		return __nativeException;
	}

	function get_stack():CallStack {
		return switch __exceptionStack {
			case null:
				var nativeTrace = NativeStackTrace.complementTrace(__nativeException.getTrace(), native);
				__exceptionStack = NativeStackTrace.toHaxe(nativeTrace, __skipStack);
			case s: s;
		}
	}
}

@:dox(hide)
@:noCompletion
@:native('Exception')
private extern class NativeException {
	@:noCompletion private function new(?message:String, ?code:Int, ?previous:NativeException):Void;

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