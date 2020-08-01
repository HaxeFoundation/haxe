package haxe;

import java.NativeArray;
import java.lang.Throwable;
import java.lang.RuntimeException;
import java.lang.StackTraceElement;
import java.io.PrintStream;
import java.io.PrintWriter;

@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeException:Throwable;
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
			var native = (value:Exception).__nativeException;
			return Std.isOfType(native, RuntimeException) ? native : value;
		} else if(Std.isOfType(value, RuntimeException)) {
			return value;
		} else if(Std.isOfType(value, Throwable)) {
			return new Exception((value:Throwable).getMessage(), null, value);
		} else {
			var e = new ValueException(value);
			var stack = e.getStackTrace();
			if(stack.length > 1) {
				e.setStackTrace(java.util.Arrays.copyOfRange(stack, 1, stack.length));
			}
			return e;
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		super(message, cast previous);
		__previousException = previous;
		if(native != null && Std.isOfType(native, Throwable)) {
			__nativeException = native;
			setStackTrace(__nativeException.getStackTrace());
		} else {
			__nativeException = cast this;
		}
	}

	function unwrap():Any {
		return __nativeException;
	}

	override public function toString():String {
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
				__exceptionStack = NativeStackTrace.toHaxe(__nativeException.getStackTrace());
			case s: s;
		}
	}
}

@:dox(hide)
@:noCompletion
@:native('java.lang.RuntimeException')
private extern class NativeException {
	@:noCompletion private function new(?message:String, ?cause:Throwable):Void;

	@:noCompletion @:skipReflection private function addSuppressed (param1:Throwable):Void;
	@:noCompletion @:skipReflection private function fillInStackTrace ():Throwable;
	@:noCompletion @:skipReflection private function getCause ():Throwable;
	@:noCompletion @:skipReflection private function getLocalizedMessage ():String;
	@:noCompletion @:skipReflection private function getMessage ():String;
	@:noCompletion @:skipReflection private function getStackTrace ():NativeArray<StackTraceElement>;
	@:noCompletion @:skipReflection private function getSuppressed ():NativeArray<Throwable>;
	@:noCompletion @:skipReflection private function initCause (param1:Throwable):Throwable;
	@:noCompletion @:skipReflection @:overload private function printStackTrace (param1:PrintWriter):Void;
	@:noCompletion @:skipReflection @:overload private function printStackTrace ():Void;
	@:noCompletion @:skipReflection @:overload private function printStackTrace (param1:PrintStream):Void;
	@:noCompletion @:skipReflection private function setStackTrace (param1:NativeArray<StackTraceElement>):Void;
	@:noCompletion @:skipReflection private function toString ():String;
}