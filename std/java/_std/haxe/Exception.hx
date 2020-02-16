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

	static public function caught(value:Any):Exception {
		if(Std.is(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, Throwable)) {
			return new Exception((value:Throwable).getMessage(), null, value);
		} else {
			return new ValueException(value);
		}
	}

	static public function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			var native = (value:Exception).__nativeException;
			return Std.isOfType(native, RuntimeException) ? native : value;
		} else if(Std.isOfType(value, RuntimeException)) {
			return value;
		} else if(Std.isOfType(value, Throwable)) {
			return new Exception((value:Throwable).getMessage(), null, value);
		} else {
			return new ValueException(value);
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		super(message, previous);
		__previousException = previous;
		if(native != null && Std.isOfType(native, Throwable)) {
			__nativeException = native;
			setStackTrace(__nativeException.getStackTrace());
		} else {
			__nativeException = cast this;
		}
	}

	public function unwrap():Any {
		return __nativeException;
	}

	override public function toString():String {
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
				__exceptionStack = CallStack.makeStack(__nativeException.getStackTrace());
			case s: s;
		}
	}
}

@:dox(hide)
@:noCompletion
@:native('java.lang.RuntimeException')
private extern class NativeException {
	@:noCompletion private function new(?message:String, ?cause:NativeException):Void;

	@:noCompletion private function addSuppressed (param1:Throwable):Void;
	@:noCompletion private function fillInStackTrace ():Throwable;
	@:noCompletion private function getCause ():Throwable;
	@:noCompletion private function getLocalizedMessage ():String;
	@:noCompletion private function getMessage ():String;
	@:noCompletion private function getStackTrace ():NativeArray<StackTraceElement>;
	@:noCompletion private function getSuppressed ():NativeArray<Throwable>;
	@:noCompletion private function initCause (param1:Throwable):Throwable;
	@:noCompletion @:overload private function printStackTrace (param1:PrintWriter):Void;
	@:noCompletion @:overload private function printStackTrace ():Void;
	@:noCompletion @:overload private function printStackTrace (param1:PrintStream):Void;
	@:noCompletion private function setStackTrace (param1:NativeArray<StackTraceElement>):Void;
	@:noCompletion private function toString ():String;
}