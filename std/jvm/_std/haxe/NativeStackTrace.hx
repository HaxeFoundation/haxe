package haxe;

import java.NativeArray;
import java.lang.ThreadLocal;
import java.lang.Throwable;
import java.lang.Thread;
import java.lang.StackTraceElement;
import haxe.CallStack.StackItem;

private typedef NativeTrace = NativeArray<StackTraceElement>;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	// named exceptionStore (not exception) to avoid shadowing the saveStack parameter
	static var exceptionStore = new ThreadLocal<Throwable>();

	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(exception:Any):Void {
		exceptionStore.set((exception : Throwable));
	}

	static public function callStack():NativeTrace {
		var stack = Thread.currentThread().getStackTrace();
		return stack.length <= 3 ? stack : java.util.Arrays.copyOfRange(stack, 3, stack.length);
	}

	static public function exceptionStack():NativeTrace {
		return switch exceptionStore.get() {
			case null: new NativeArray(0);
			case e: e.getStackTrace();
		}
	}

	static public function toHaxe(nativeStackTrace:NativeTrace, skip:Int = 0):Array<StackItem> {
		var stack = [];
		for (i in 0...nativeStackTrace.length) {
			if(skip > i) {
				continue;
			}
			var el = nativeStackTrace[i];
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