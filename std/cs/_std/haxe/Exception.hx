package haxe;

import cs.system.Exception as CsException;
import php.NativeAssocArray;
import php.NativeIndexedArray;

class ValueException extends Exception {
	public var value(default,null):Any;

	public function new(value:Any, ?previous:Exception):Void {
		super(inline Std.string(value), previous);
		this.value = value;
	}

	override public function unwrap():Any {
		return value;
	}
}

@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeException:CsException;
	@:noCompletion var __previousException:Null<Exception>;

	static public function wrap(value:Any):Exception {
		if(Std.is(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, CsException)) {
			return new Exception((value:CsException).Message, null, value);
		} else {
			return new ValueException(value);
		}
	}

	static public function wrapNative(value:Any):Any {
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
		this.__nativeException = native == null ? cast this : native;
	}

	public function unwrap():Any {
		return __nativeException;
	}

	public function toString():String {
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
				var nativeTrace = new cs.system.diagnostics.StackTrace(native, true)
				__exceptionStack = CallStack.makeStack(nativeTrace);
			case s: s;
		}
	}
}

@:dox(hide)
@:noCompletion
@:native('System.Exception')
private extern class NativeException {
	@:noCompletion private function new(message:String, innerException:NativeException):Void;
	@:noCompletion private final Data:cs.system.collections.IDictionary;
	@:noCompletion private var HelpLink:String;
	@:noCompletion private final InnerException:cs.system.Exception;
	@:noCompletion private final Message:String;
	@:noCompletion private var Source:String;
	@:noCompletion private final StackTrace:String;
	@:noCompletion private final TargetSite:cs.system.reflection.MethodBase;
	@:overload @:noCompletion private function GetBaseException():cs.system.Exception;
	@:overload @:noCompletion private function GetObjectData(incs.system.fo:SerializationInfo, context:StreamingContext):Void;
	@:overload @:noCompletion private function GetType():cs.system.Type;
	@:overload @:noCompletion private function ToString():cs.system.String;
}