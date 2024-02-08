package haxe;

@:coreApi
class Exception {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __exceptionMessage:String;
	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeStack:CallStack;
	@:noCompletion @:ifFeature("haxe.Exception.get_stack") var __skipStack:Int = 0;
	@:noCompletion var __nativeException:Any;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else {
			var e = new ValueException(value, null, value);
			// Undo automatic __shiftStack()
			e.__unshiftStack();
			return e;
		}
	}

	static function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			return (value:Exception).native;
		} else {
			var e = new ValueException(value);
			e.__shiftStack();
			return e;
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		__exceptionMessage = message;
		__previousException = previous;
		if(native != null) {
			__nativeStack = NativeStackTrace.exceptionStack();
			__nativeException = native;
		} else {
			__nativeStack = NativeStackTrace.callStack();
			__nativeException = this;
		}
	}

	function unwrap():Any {
		return __nativeException;
	}

	@:ifFeature("haxe.Exception.thrown")
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

	@:noCompletion
	@:ifFeature("haxe.Exception.get_stack")
	inline function __unshiftStack():Void {
		__skipStack--;
	}

	function get_message():String {
		return __exceptionMessage;
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
				__exceptionStack = if(__skipStack > 0) {
					__nativeStack.asArray().slice(__skipStack);
				} else {
					__nativeStack;
				}
			case s: s;
		}
	}
}