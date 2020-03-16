package haxe;

import java.NativeArray;
import java.lang.ThreadLocal;
import java.lang.Throwable;
import java.lang.Thread;
import java.lang.StackTraceElement;
import haxe.CallStack.StackItem;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	static var exception = new ThreadLocal<Throwable>();

	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(e:Throwable):Void {
		exception.set(e);
	}

	static public function callStack():NativeArray<StackTraceElement> {
		var stack = Thread.currentThread().getStackTrace();
		return stack.length <= 3 ? stack : java.util.Arrays.copyOfRange(stack, 3, stack.length);
	}

	static public function exceptionStack():NativeArray<StackTraceElement> {
		return switch exception.get() {
			case null: new NativeArray(0);
			case e: e.getStackTrace();
		}
	}

	static public function toHaxe(native:NativeArray<StackTraceElement>, skip:Int = 0):Array<StackItem> {
		var stack = [];
		for (i in 0...native.length) {
			if(skip > i) {
				continue;
			}
			var el = native[i];
			var className = el.getClassName();
			var methodName = el.getMethodName();
			var fileName = el.getFileName();
			var lineNumber = el.getLineNumber();
			var method = Method(className, methodName);
			if (fileName != null || lineNumber >= 0) {
				stack.push(FilePos(method, fileName, lineNumber));
			} else {
				stack.push(method);
			}
		}
		return stack;
	}
}