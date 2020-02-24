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

	static public inline function saveStack(e:Throwable):Void {
		exception.set(e);
	}

	static public inline function callStack():NativeArray<StackTraceElement> {
		return Thread.currentThread().getStackTrace();
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