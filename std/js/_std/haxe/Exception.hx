package haxe;

import js.lib.Error;

@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:noCompletion @:ifFeature("haxe.Exception.stack") var __skipStack:Int;
	@:noCompletion var __exceptionStack(get,set):Null<CallStack>;
	@:noCompletion var __nativeException:Any;
	@:noCompletion var __previousException:Null<Exception>;

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
			var e = new ValueException(value);
			untyped __feature__('haxe.Exception.stack', e.__shiftStack());
			return e;
		}
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		super(message);
		(cast this).message = message;
		__previousException = previous;
		__nativeException = native != null ? native : this;
		untyped __feature__('haxe.Exception.stack', {
			__skipStack = 0;
			var old = Syntax.code('Error.prepareStackTrace');
			Syntax.code('Error.prepareStackTrace = function(e) { return e.stack; }');
			if(Std.isOfType(native, Error)) {
				(cast this).stack = NativeStackTrace.normalize(native.stack);
			} else {
				var e:Error = if ((cast Error).captureStackTrace) {
					(cast Error).captureStackTrace(this, Exception);
					cast this;
				} else {
					new Error();
				}
				(cast this).stack = NativeStackTrace.normalize(e.stack);
			}
			Syntax.code('Error.prepareStackTrace = {0}', old);
		});
	}

	public function unwrap():Any {
		return __nativeException;
	}

	public function toString():String {
		return inline CallStack.exceptionToString(this);
	}

	@:noCompletion
	@:ifFeature("haxe.Exception.stack")
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
				__exceptionStack = NativeStackTrace.toHaxe((cast this).stack, __skipStack);
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

	inline function get___exceptionStack():CallStack {
		return (cast this).__exceptionStack;
	}

	inline function set___exceptionStack(value:CallStack):CallStack {
		setProperty('__exceptionStack', value);
		return value;
	}

	inline function get___skipStack():Int {
		return (cast this).__skipStack;
	}

	inline function set___skipStack(value:Int):Int {
		setProperty('__skipStack', value);
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