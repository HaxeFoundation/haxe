/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package haxe;

@:coreApi
class Exception extends NativeException {
	public var message(get, never):String;
	public var stack(get, set):CallStack;
	public var previous(get, never):Null<Exception>;
	public var native(get, never):Any;

	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeException:NativeException;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		if (Std.isOfType(value, Exception)) {
			return cast value;
		}
		// Check if it's a native System.Exception - use inline C# to get message
		if (untyped __cs__("{0} is System.Exception", value)) {
			return new Exception(untyped __cs__("((System.Exception){0}).Message", value), null, value);
		}
		return new ValueException(value, null, value);
	}

	static function thrown(value:Any):Any {
		if (Std.isOfType(value, Exception)) {
			var native = (cast value : Exception).__nativeException;
			if (untyped __cs__("{0} is System.Exception", native)) {
				return native;
			}
			return value;
		}
		if (untyped __cs__("{0} is System.Exception", value)) {
			return value;
		}
		return new ValueException(value);
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		// Call base System.Exception constructor - uses special handling in generator
		super(message);
		__previousException = previous;
		// native is Null<object> struct - check .hasValue and .value
		__nativeException = untyped __cs__("(({0}.hasValue && {0}.value is System.Exception) ? (System.Exception){0}.value : (System.Exception)this)", native);
	}

	function unwrap():Any {
		return __nativeException;
	}

	public function toString():String {
		return message;
	}

	public function details():String {
		return CallStack.exceptionToString(this);
	}

	function get_message():String {
		return untyped __cs__("base.Message");
	}

	function get_previous():Null<Exception> {
		return __previousException;
	}

	final function get_native():Any {
		return __nativeException;
	}

	function get_stack():CallStack {
		if (__exceptionStack == null) {
			// Create a StackTrace from the native exception - like JVM does with getStackTrace()
			// Cast needed because __nativeException is typed as private NativeException class
			var nativeEx:cs.system.Exception = cast __nativeException;
			var stackTrace = new cs.system.diagnostics.StackTrace(nativeEx, true);
			__exceptionStack = NativeStackTrace.toHaxe(stackTrace);
		}
		return __exceptionStack;
	}

	function set_stack(stack:CallStack):CallStack {
		__exceptionStack = stack;
		return stack;
	}
}

@:dox(hide)
@:noCompletion
@:native('System.Exception')
private extern class NativeException {
	@:noCompletion @:overload private function new(message:String):Void;
	@:noCompletion @:overload private function new(message:String, innerException:NativeException):Void;
	@:noCompletion @:skipReflection private var Message(default, null):String;
	@:noCompletion @:skipReflection private var InnerException(default, null):NativeException;
	@:noCompletion @:skipReflection private var StackTrace(default, null):String;
}
