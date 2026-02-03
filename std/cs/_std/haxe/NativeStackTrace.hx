package haxe;

import haxe.CallStack.StackItem;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static var exception:cs.system.Exception;

	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(e:cs.system.Exception):Void {
		exception = e;
	}

	static public function callStack():cs.system.diagnostics.StackTrace {
		return new cs.system.diagnostics.StackTrace(true);
	}

	static public function exceptionStack():Null<cs.system.diagnostics.StackTrace> {
		return switch exception {
			case null: null;
			case e: new cs.system.diagnostics.StackTrace(e, true);
		}
	}

	static public function toHaxe(native:cs.system.diagnostics.StackTrace, skip:Int = 0):Array<StackItem> {
		var stack:Array<StackItem> = [];
		if (native == null) return stack;

		var frameCount = native.FrameCount;
		for (i in 0...frameCount) {
			if (skip > i) {
				continue;
			}
			var frame = native.GetFrame(i);
			if (frame == null) continue;

			var method = frame.GetMethod();
			var className:String = method != null ? untyped __cs__("{0}.ReflectedType?.ToString() ?? \"Unknown\"", method) : "Unknown";
			var methodName = method != null ? method.Name : "Unknown";

			// Skip internal NativeStackTrace frames (appear in JIT but not AOT)
			if (className == "haxe.NativeStackTrace") continue;
			var fileName = frame.GetFileName();
			var lineNumber = frame.GetFileLineNumber();

			var stackMethod = Method(className, methodName);
			if (fileName != null || lineNumber > 0) {
				stack.push(FilePos(stackMethod, fileName, lineNumber));
			} else {
				stack.push(stackMethod);
			}
		}
		return stack;
	}
}
