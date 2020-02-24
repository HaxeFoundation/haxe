package haxe;

import cs.system.Exception as CsException;


@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeException:CsException;
	@:noCompletion var __previousException:Null<Exception>;

	static public function caught(value:Any):Exception {
		if(Std.is(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, CsException)) {
			return new Exception((value:CsException).Message, null, value);
		} else {
			return new ValueException(value, null, value);
		}
	}

	static public function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			return (value:Exception).native;
		} else if(Std.isOfType(value, CsException)) {
			return value;
		} else {
			return new ValueException(value);
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		super(message, previous);
		this.__previousException = previous;
		if(native != null && Std.isOfType(native, CsException)) {
			__nativeException = native;
		} else {
			__nativeException = cast this;
		}
	}

	public function unwrap():Any {
		return __nativeException;
	}

	public function toString():String {
		return inline CallStack.exceptionToString(this);
	}

	function get_message():String {
		return this.Message;
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
				var nativeTrace = new cs.system.diagnostics.StackTrace(__nativeException, true);
				__exceptionStack = NativeStackTrace.toHaxe(nativeTrace);
			case s: s;
		}
	}
}

@:dox(hide)
@:nativeGen
@:noCompletion
@:native('System.Exception')
private extern class NativeException {
	@:noCompletion private function new(message:String, innerException:NativeException):Void;
	@:noCompletion @:skipReflection private final Data:cs.system.collections.IDictionary;
	@:noCompletion @:skipReflection private var HelpLink:String;
	@:noCompletion @:skipReflection private final InnerException:cs.system.Exception;
	@:noCompletion @:skipReflection private final Message:String;
	@:noCompletion @:skipReflection private var Source:String;
	@:noCompletion @:skipReflection private final StackTrace:String;
	@:noCompletion @:skipReflection private final TargetSite:cs.system.reflection.MethodBase;
	@:overload @:noCompletion @:skipReflection private function GetBaseException():cs.system.Exception;
	@:overload @:noCompletion @:skipReflection private function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	@:overload @:noCompletion @:skipReflection private function GetType():cs.system.Type;
	@:overload @:noCompletion @:skipReflection private function ToString():cs.system.String;
}