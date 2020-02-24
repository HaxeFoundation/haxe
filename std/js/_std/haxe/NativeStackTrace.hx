package haxe;

import js.lib.Error;
import haxe.CallStack.StackItem;

// https://v8.dev/docs/stack-trace-api
@:native("Error")
private extern class V8Error {
	static var prepareStackTrace:(error:Error, structuredStackTrace:Array<V8CallSite>)->Any;
}

typedef V8CallSite = {
	function getFunctionName():String;
	function getFileName():String;
	function getLineNumber():Int;
	function getColumnNumber():Int;
}

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	static var lastError:Error;

	// support for source-map-support module
	@:noCompletion
	public static var wrapCallSite:V8CallSite->V8CallSite;

	static public inline function saveStack(e:Error):Void {
		lastError = e;
	}

	static public inline function callStack():Any {
		return tryHaxeStack(new Error());
	}

	static public function exceptionStack():Any {
		return tryHaxeStack(lastError);
	}

	static public function toHaxe(s:Null<Any>, skip:Int = 0):Array<StackItem> {
		if (s == null) {
			return [];
		} else if (js.Syntax.typeof(s) == "string") {
			// Return the raw lines in browsers that don't support prepareStackTrace
			var stack:Array<String> = (s:String).split("\n");
			if (stack[0] == "Error")
				stack.shift();
			var m = [];
			var rie10 = ~/^    at ([A-Za-z0-9_. ]+) \(([^)]+):([0-9]+):([0-9]+)\)$/;
			for (i in 0...stack.length) {
				if(skip > i) continue;
				var line = stack[i];
				if (rie10.match(line)) {
					var path = rie10.matched(1).split(".");
					var meth = path.pop();
					var file = rie10.matched(2);
					var line = Std.parseInt(rie10.matched(3));
					var column = Std.parseInt(rie10.matched(4));
					m.push(FilePos(meth == "Anonymous function" ? LocalFunction() : meth == "Global code" ? null : Method(path.join("."), meth), file, line,
						column));
				} else
					m.push(Module(StringTools.trim(line))); // A little weird, but better than nothing
			}
			return m;
		} else if(skip > 0 && js.Syntax.code('Array.isArray({0})', s)) {
			return (s:Array<StackItem>).slice(skip);
		} else {
			return cast s;
		}
	}

	static function getJsStack(e:Error):String {
		var old = V8Error.prepareStackTrace;
		V8Error.prepareStackTrace = prepareJsStackTrace;
		var stack = e.stack;
		V8Error.prepareStackTrace = old;
		return stack;
	}

	static function prepareJsStackTrace(error:Error, callsites:Array<V8CallSite>):Any {
		return error.stack;
	}

	static function tryHaxeStack(e:Null<Error>):Any {
		if (e == null) {
			return [];
		}
		// https://v8.dev/docs/stack-trace-api
		var oldValue = V8Error.prepareStackTrace;
		V8Error.prepareStackTrace = prepareHxStackTrace;
		var stack = e.stack;
		V8Error.prepareStackTrace = oldValue;
		return stack;
	}

	static function prepareHxStackTrace(error:Error, callsites:Array<V8CallSite>):Any {
		var stack = [];
		for (site in callsites) {
			if (wrapCallSite != null)
				site = wrapCallSite(site);
			var method = null;
			var fullName = site.getFunctionName();
			if (fullName != null) {
				var idx = fullName.lastIndexOf(".");
				if (idx >= 0) {
					var className = fullName.substr(0, idx);
					var methodName = fullName.substr(idx + 1);
					method = Method(className, methodName);
				}
			}
			var fileName = site.getFileName();
			var fileAddr = fileName == null ? -1 : fileName.indexOf("file:");
			if (wrapCallSite != null && fileAddr > 0)
				fileName = fileName.substr(fileAddr + 6);
			stack.push(FilePos(method, fileName, site.getLineNumber(), site.getColumnNumber()));
		}
		return stack;
	}
}