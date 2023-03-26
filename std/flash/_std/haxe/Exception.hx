package haxe;

import flash.errors.Error;

@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeStack:String;
	@:noCompletion @:ifFeature("haxe.Exception.get_stack") var __skipStack:Int;
	@:noCompletion var __nativeException:Error;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, Error)) {
			return new Exception((value:Error).message, null, value);
		} else {
			return new ValueException(value, null, value);
		}
	}

	static function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			return (value:Exception).native;
		} else if(Std.isOfType(value, Error)) {
			return value;
		} else {
			var e = new ValueException(value);
			e.__shiftStack();
			return e;
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		super(message);
		__previousException = previous;
		if(native != null && Std.isOfType(native, Error)) {
			__nativeException = native;
			__nativeStack = NativeStackTrace.normalize((native:Error).getStackTrace());
		} else {
			__nativeException = cast this;
			__nativeStack = NativeStackTrace.callStack();
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

	@:noCompletion
	@:ifFeature("haxe.Exception.get_stack")
	inline function __shiftStack():Void {
		__skipStack++;
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
		return switch __exceptionStack {
			case null:
				__exceptionStack = NativeStackTrace.toHaxe(__nativeStack, __skipStack);
			case s: s;
		}
	}
}

@:dox(hide)
@:native('flash.errors.Error')
extern class NativeException {
	@:noCompletion @:flash.property private var errorID(get,never):Int;
	// @:noCompletion private var message:Dynamic;
	@:noCompletion private var name:Dynamic;
	@:noCompletion private function new(?message:Dynamic, id:Dynamic = 0):Void;
	@:noCompletion private function getStackTrace():String;
	@:noCompletion private function get_errorID():Int;
}