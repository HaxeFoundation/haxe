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
	@:noCompletion var __nativeStack:cs.system.diagnostics.StackTrace;
	@:noCompletion var __ownStack:Bool;
	@:noCompletion var __skipStack:Int = 0;
	@:noCompletion var __nativeException:NativeException;
	@:noCompletion var __previousException:Null<Exception>;

	static function caught(value:Any):Exception {
		// Use direct C# is check for AOT compatibility
		if (cs.Syntax.code("{0} is haxe.Exception", value)) {
			return cs.Syntax.code("(haxe.Exception){0}", value);
		}
		// Check if it's a native System.Exception - use inline C# to get message
		if (cs.Syntax.code("{0} is global::System.Exception", value)) {
			return new Exception(cs.Syntax.code("((global::System.Exception){0}).Message", value), null, value);
		}
		return new ValueException(value, null, value);
	}

	static function thrown(value:Any):Any {
		// Use direct C# is check for AOT compatibility
		if (cs.Syntax.code("{0} is haxe.Exception", value)) {
			return cs.Syntax.code("((haxe.Exception){0}).__nativeException", value);
		}
		if (cs.Syntax.code("{0} is global::System.Exception", value)) {
			return value;
		}
		// Note: Don't call __shiftStack() here - the leading constructor filtering
		// in NativeStackTrace.toHaxe handles skipping the thrown() frame
		return new ValueException(value);
	}

	public function new(message:String, ?previous:Exception, ?native:Any) {
		// Call base System.Exception constructor - uses special handling in generator
		super(message);
		__previousException = previous;

		// Capture stack trace and native exception like Haxe4 does
		if (native != null && cs.Syntax.code("{0} is global::System.Exception", native)) {
			__nativeException = cs.Syntax.code("(global::System.Exception){0}", native);
			// Check if the native exception has a stack trace
			var hasStack:Bool = cs.Syntax.code("((global::System.Exception){0}).StackTrace != null", native);
			if (hasStack) {
				__nativeStack = new cs.system.diagnostics.StackTrace(cast __nativeException, true);
				__ownStack = false;
			} else {
				// Exception has no stack trace, capture current call stack (skip 1 for constructor)
				__nativeStack = cs.Syntax.code("new global::System.Diagnostics.StackTrace(1, true)");
				__ownStack = true;
			}
		} else {
			__nativeException = cast this;
			// Capture current call stack (skip 1 for constructor)
			__nativeStack = cs.Syntax.code("new global::System.Diagnostics.StackTrace(1, true)");
			__ownStack = true;
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

	@:noCompletion
	inline function __shiftStack():Void {
		if (__ownStack) __skipStack++;
	}

	function get_message():String {
		return cs.Syntax.code("base.Message");
	}

	function get_previous():Null<Exception> {
		return __previousException;
	}

	final function get_native():Any {
		return __nativeException;
	}

	function get_stack():CallStack {
		if (__exceptionStack == null) {
			try {
				if (__nativeStack != null) {
					__exceptionStack = NativeStackTrace.toHaxe(__nativeStack, __skipStack);
				}
			} catch (e:Dynamic) {
				// Fallback to empty stack on any error
			}
			if (__exceptionStack == null) {
				__exceptionStack = [];
			}
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
	@:noCompletion @:unreflective private var Message(default, null):String;
	@:noCompletion @:unreflective private var InnerException(default, null):NativeException;
	@:noCompletion @:unreflective private var StackTrace(default, null):String;
}
