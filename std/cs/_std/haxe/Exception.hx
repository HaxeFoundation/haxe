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
class Exception {
	public var message(get, never):String;
	public var stack(get, set):CallStack;
	public var previous(get, never):Null<Exception>;
	public var native(get, never):Any;

	@:noCompletion var __exceptionMessage:String;
	@:noCompletion var __exceptionStack:Null<CallStack>;
	@:noCompletion var __nativeException:Dynamic;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		if (Std.isOfType(value, Exception)) {
			return cast value;
		}
		// Check if it's a native System.Exception
		if (untyped __cs__("{0} is System.Exception", value)) {
			var native:Dynamic = value;
			return new Exception(untyped __cs__("{0}.Message", native), null, native);
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
		__exceptionMessage = message;
		__previousException = previous;
		if (native != null) {
			__nativeException = native;
		} else {
			__nativeException = this;
		}
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
		return __exceptionMessage;
	}

	function get_previous():Null<Exception> {
		return __previousException;
	}

	final function get_native():Any {
		return __nativeException;
	}

	function get_stack():CallStack {
		if (__exceptionStack == null) {
			__exceptionStack = NativeStackTrace.toHaxe(__nativeException);
		}
		return __exceptionStack;
	}

	function set_stack(stack:CallStack):CallStack {
		__exceptionStack = stack;
		return stack;
	}
}
