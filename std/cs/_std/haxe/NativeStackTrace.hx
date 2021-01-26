package haxe;

import haxe.CallStack.StackItem;
import cs.system.diagnostics.StackTrace;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	@:meta(System.ThreadStaticAttribute)
	static var exception:Null<cs.system.Exception>;

	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(e:Any):Void {
		exception = e;
	}

	static public inline function callStack():StackTrace {
		return new StackTrace(1, true);
	}

	static public function exceptionStack():Null<StackTrace> {
		return switch exception {
			case null: null;
			case e: new StackTrace(e, true);
		}
	}

	static public function toHaxe(native:Null<StackTrace>, skip:Int = 0):Array<StackItem> {
		var stack = [];
		if(native == null) {
			return stack;
		}
		var cnt = 0;
		for (i in 0...native.FrameCount) {
			var frame = native.GetFrame(i);
			var m = frame.GetMethod();

			if (m == null) {
				continue;
			}
			if(skip > cnt++) {
				continue;
			}

			var method = StackItem.Method(m.ReflectedType.ToString(), m.Name);

			var fileName = frame.GetFileName();
			var lineNumber = frame.GetFileLineNumber();

			if (fileName != null || lineNumber >= 0)
				stack.push(FilePos(method, fileName, lineNumber));
			else
				stack.push(method);
		}
		return stack;
	}
}