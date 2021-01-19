package haxe;

import js.lib.Error;

@:coreApi
class Exception extends NativeException {
	public var message(get,never):String;
	public var stack(get,never):CallStack;
	public var previous(get,never):Null<Exception>;
	public var native(get,never):Any;

	@:ifFeature("haxe.Exception.get_stack")
	@:noCompletion var __skipStack:Int;
	@:noCompletion var __exceptionStack(get,set):Null<CallStack>;
	@:noCompletion var __nativeException:Any;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		if(Std.isOfType(value, Exception)) {
			return value;
		} else if(Std.isOfType(value, Error)) {
			return new Exception((cast value:Error).message, null, value);
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
			untyped __feature__("haxe.Exception.get_stack", e.__shiftStack());
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
			var old = js.Syntax.code('Error.prepareStackTrace');
			js.Syntax.code('Error.prepareStackTrace = function(e) { return e.stack; }');
			if(Std.isOfType(native, Error)) {
				(cast this).stack = native.stack;
			} else {
				var e:Error = null;
				if ((cast Error).captureStackTrace) {
					(cast Error).captureStackTrace(this, Exception);
					e = cast this;
				} else {
					e = new Error();
					//Internet Explorer provides call stack only if error was thrown
					if(js.Syntax.typeof(e.stack) == "undefined") {
						js.Syntax.code('try { throw {0}; } catch(_) {}', e);
						__skipStack++;
					}
				}
				(cast this).stack = e.stack;
			}
			js.Syntax.code('Error.prepareStackTrace = {0}', old);
		});
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

	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	function get_stack():CallStack {
		return switch __exceptionStack {
			case null:
				__exceptionStack = NativeStackTrace.toHaxe(NativeStackTrace.normalize((cast this).stack), __skipStack);
			case s: s;
		}
	}

	@:noCompletion
	function setProperty(name:String, value:Any):Void {
		try {
			js.lib.Object.defineProperty(this, name, {value:value});
		} catch(e:Exception) {
			js.Syntax.code('{0}[{1}] = {2}', this, name, value);
		}
	}

	@:noCompletion
	inline function get___exceptionStack():CallStack {
		return (cast this).__exceptionStack;
	}

	@:noCompletion
	inline function set___exceptionStack(value:CallStack):CallStack {
		setProperty('__exceptionStack', value);
		return value;
	}

	@:noCompletion
	inline function get___skipStack():Int {
		return (cast this).__skipStack;
	}

	@:noCompletion
	inline function set___skipStack(value:Int):Int {
		setProperty('__skipStack', value);
		return value;
	}

	@:noCompletion
	inline function get___nativeException():Any {
		return (cast this).__nativeException;
	}

	@:noCompletion
	inline function set___nativeException(value:Any):Any {
		setProperty('__nativeException', value);
		return value;
	}

	@:noCompletion
	inline function get___previousException():Null<Exception> {
		return (cast this).__previousException;
	}

	@:noCompletion
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
	// private var stack(default, null):String; //redefined in haxe.Exception

	function new(?message:String);
}
