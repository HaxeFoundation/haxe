package haxe;

import js.lib.Error;

@:coreApi
@:access(haxe.NativeStackTrace)
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion var __errorStack(get,set):Null<CallStack>;
	@:noCompletion var __nativeException(get,set):Any;
	@:noCompletion var __previousException(get,set):Null<Exception>;

	static public function caught(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, Error)) {
			return new Exception((cast value:Error).message, null, value);
		} else {
			return new ValueException(value, null, value);
		}
	}

	static public function thrown(value:Any):Any {
		if(Std.isOfType(value, Exception)) {
			return (value:Exception).native;
		} else if(Std.isOfType(value, Error)) {
			return value;
		} else {
			return new ValueException(value);
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		super(message);
		(cast this).message = message;
		__previousException = previous;
		__nativeException = native != null ? native : this;
		var e:Error = if(Std.isOfType(native, Error)) {
			native;
		} else if ((cast Error).captureStackTrace) {
			(cast Error).captureStackTrace(this, Exception);
			cast this;
		} else {
			new Error();
		}
		(cast this).stack = NativeStackTrace.getJsStack(e);
	}

	public function unwrap():Any {
		return __nativeException;
	}

	public function toString():String {
		return inline CallStack.exceptionToString(this);
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
		return switch __errorStack {
			case null:
				__errorStack = NativeStackTrace.toHaxe(NativeStackTrace.getJsStack(cast this));
			case s: s;
		}
	}

	function setProperty(name:String, value:Any):Void {
		try {
			js.lib.Object.defineProperty(this, name, {value:value});
		} catch(e:Exception) {
			js.Syntax.code('{0}[{1}] = {2}', this, name, value);
		}
	}

	inline function get___errorStack():CallStack {
		return (cast this).__errorStack;
	}

	inline function set___errorStack(value:CallStack):CallStack {
		setProperty('__errorStack', value);
		return value;
	}

	inline function get___nativeException():Any {
		return (cast this).__nativeException;
	}

	inline function set___nativeException(value:Any):Any {
		setProperty('__nativeException', value);
		return value;
	}

	inline function get___previousException():Null<Exception> {
		return (cast this).__previousException;
	}

	inline function set___previousException(value:Null<Exception>):Null<Exception> {
		setProperty('__previousException', value);
		return value;
	}
}

@:dox(hide)
@:noCompletion
@:native('Error')
private extern class NativeException {
	// private var message:String; //redefined in haxe.Exception
	@:noCompletion private var name:String;
	// private var stack(default, null):String; //redefined in haxe.Exception

	private function new(?message:String):Void;
}