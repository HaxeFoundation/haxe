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

	static public inline function callStack():cs.system.diagnostics.StackTrace {
		return untyped __cs__("new System.Diagnostics.StackTrace(1, true)");
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

		var frameCount:Int = untyped __cs__("{0}?.FrameCount ?? 0", native);
		if (frameCount == 0) return stack;

		var cnt = 0;

		// First pass: check if we have any non-constructor frames
		var hasNonCtorFrame = false;
		for (i in 0...frameCount) {
			try {
				var frame = native.GetFrame(i);
				if (frame == null) continue;
				var m = frame.GetMethod();
				if (m == null) continue;
				if (skip > cnt++) continue;

				var methodName:String = untyped __cs__("{0}?.Name ?? \"\"", m);
				var className:String = untyped __cs__("{0}?.ReflectedType?.ToString() ?? \"Unknown\"", m);
				var isHaxeException:Bool = untyped __cs__("{0}?.StartsWith(\"haxe.Exception\") ?? false", className);

				if (methodName != ".ctor" && !(methodName == "thrown" && isHaxeException)) {
					hasNonCtorFrame = true;
					break;
				}
			} catch (e:Dynamic) {
				continue;
			}
		}

		// Second pass: build stack, only filter ctors if we have non-ctor frames
		var passedLeadingCtors = !hasNonCtorFrame; // If no non-ctor frames, don't filter at all
		cnt = 0;

		for (i in 0...frameCount) {
			try {
				var frame = native.GetFrame(i);
				if (frame == null) continue;
				var m = frame.GetMethod();
				if (m == null) continue;
				if (skip > cnt++) continue;

				var className:String = untyped __cs__("{0}?.ReflectedType?.ToString() ?? \"Unknown\"", m);
				var methodName:String = untyped __cs__("{0}?.Name ?? \"\"", m);

				// Skip leading constructor and thrown frames only if we have non-ctor frames
				if (!passedLeadingCtors) {
					if (methodName == ".ctor") continue;
					var isHaxeException:Bool = untyped __cs__("{0}?.StartsWith(\"haxe.Exception\") ?? false", className);
					if (methodName == "thrown" && isHaxeException) continue;
					passedLeadingCtors = true;
				}

				var method = StackItem.Method(className, methodName);

				var fileName:String = untyped __cs__("{0}?.GetFileName()", frame);
				var lineNumber:Int = untyped __cs__("{0}?.GetFileLineNumber() ?? 0", frame);

				if (fileName != null || lineNumber >= 0)
					stack.push(FilePos(method, fileName, lineNumber));
				else
					stack.push(method);
			} catch (e:Dynamic) {
				continue;
			}
		}
		return stack;
	}
}
