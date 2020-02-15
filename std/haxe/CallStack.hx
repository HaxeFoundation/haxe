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

#if php
import php.*;
import haxe.Exception in Exception;

private typedef NativeTrace = NativeIndexedArray<NativeAssocArray<Dynamic>>;
#else
private typedef NativeTrace =
	#if java java.NativeArray<java.lang.StackTraceElement>
	#elseif cs cs.system.diagnostics.StackTrace
	#else Dynamic
	#end;
#end

/**
	Elements return by `CallStack` methods.
**/
enum StackItem {
	CFunction;
	Module(m:String);
	FilePos(s:Null<StackItem>, file:String, line:Int, ?column:Null<Int>);
	Method(classname:Null<String>, method:String);
	LocalFunction(?v:Int);
}

/**
	Get information about the call stack.
**/
@:allow(haxe.Exception)
@:using(haxe.CallStack)
abstract CallStack(Array<StackItem>) from Array<StackItem> {
	/**
		The length of this stack.
	**/
	public var length(get,never):Int;
	inline function get_length():Int return this.length;

	@:arrayAccess public inline function get(index:Int):StackItem {
		return this[index];
	}

	/**
		Make a copy of the stack.
	**/
	public inline function copy():CallStack {
		return this.copy();
	}

	/**
		Returns a representation of the stack as a printable string.
	**/
	static public function toString(stack:CallStack):String {
		return inline toStringImpl(stack);
	}

	/**
		Returns a range of entries of current stack from the beginning to the the
		common part of this and `stack`.
	**/
	public function subtract(stack:CallStack):CallStack {
		var startIndex = -1;
		var i = -1;
		while(++i < this.length) {
			for(j in 0...stack.length) {
				if(equalItems(this[i], stack[j])) {
					if(startIndex < 0) {
						startIndex = i;
					}
					++i;
					if(i >= this.length) break;
				} else {
					startIndex = -1;
				}
			}
			if(startIndex >= 0) break;
		}
		return startIndex >= 0 ? this.slice(0, startIndex) : this;
	}

	static function equalItems(item1:Null<StackItem>, item2:Null<StackItem>):Bool {
		return switch([item1, item2]) {
			case [null, null]: true;
			case [CFunction, CFunction]: true;
			case [Module(m1), Module(m2)]:
				m1 == m2;
			case [FilePos(item1, file1, line1, col1), FilePos(item2, file2, line2, col2)]:
				file1 == file2 && line1 == line2 && col1 == col2 && equalItems(item1, item2);
			case [Method(class1, method1), Method(class2, method2)]:
				class1 == class2 && method1 == method2;
			case [LocalFunction(v1), LocalFunction(v2)]:
				v1 == v2;
			case _: false;
		}
	}

	/**
		Return the call stack elements, or an empty array if not available.
	**/
	public static function callStack():Array<StackItem> {
		return callStackImpl();
	}

	/**
		Return the exception stack : this is the stack elements between
		the place the last exception was thrown and the place it was
		caught, or an empty array if not available.

		May not work if catch type was a derivative from `haxe.Exception`.
	**/
	#if cpp
	@:noDebug /* Do not mess up the exception stack */
	#end
	public static function exceptionStack():Array<StackItem> {
		return exceptionStackImpl();
	}

	inline function asArray():Array<StackItem> {
		return this;
	}

	static function exceptionToString(e:Exception):String {
		if(e.previous == null) {
			return 'Exception: ${e.message}${e.stack}';
		}
		var result = '';
		var e:Null<Exception> = e;
		var prev:Null<Exception> = null;
		while(e != null) {
			if(prev == null) {
				result = 'Exception: ${e.message}${e.stack}' + result;
			} else {
				var prevStack = @:privateAccess e.stack.subtract(prev.stack);
				result = 'Exception: ${e.message}${prevStack}\n\nNext ' + result;
			}
			prev = e;
			e = e.previous;
		}
		return result;
	}

	static function toStringImpl(stack:CallStack) {
		var b = new StringBuf();
		for (s in stack.asArray()) {
			b.add('\nCalled from ');
			itemToString(b, s);
		}
		return b.toString();
	}

	static function itemToString(b:StringBuf, s) {
		switch (s) {
			case CFunction:
				b.add("a C function");
			case Module(m):
				b.add("module ");
				b.add(m);
			case FilePos(s, file, line, col):
				if (s != null) {
					itemToString(b, s);
					b.add(" (");
				}
				b.add(file);
				b.add(" line ");
				b.add(line);
				if (col != null) {
					b.add(" column ");
					b.add(col);
				}
				if (s != null)
					b.add(")");
			case Method(cname, meth):
				b.add(cname == null ? "<unknown>" : cname);
				b.add(".");
				b.add(meth);
			case LocalFunction(n):
				b.add("local function #");
				b.add(n);
		}
	}

// All target specifics should stay beyond this line.

#if php
	/**
		This method is used internally by some targets for non-haxe.Exception catches
		to provide stack for `haxe.CallStack.exceptionStack()`
	**/
	static function saveExceptionStack(e:Throwable):Void {
		var nativeTrace = e.getTrace();

		// Reduce exception stack to the place where exception was caught
		var currentTrace = Global.debug_backtrace(Const.DEBUG_BACKTRACE_IGNORE_ARGS);
		var count = Global.count(currentTrace);

		for (i in -(count - 1)...1) {
			var exceptionEntry:NativeAssocArray<Dynamic> = Global.end(nativeTrace);

			if (!Global.isset(exceptionEntry['file']) || !Global.isset(currentTrace[-i]['file'])) {
				Global.array_pop(nativeTrace);
			} else if (currentTrace[-i]['file'] == exceptionEntry['file'] && currentTrace[-i]['line'] == exceptionEntry['line']) {
				Global.array_pop(nativeTrace);
			} else {
				break;
			}
		}

		// Remove arguments from trace to avoid blocking some objects from GC
		var count = Global.count(nativeTrace);
		for (i in 0...count) {
			nativeTrace[i]['args'] = new NativeArray();
		}

		lastExceptionTrace = complementTrace(nativeTrace, e);
	}

	/**
		If defined this function will be used to transform call stack entries.
		@param String - generated php file name.
		@param Int - Line number in generated file.
	**/
	static public var mapPosition:String->Int->Null<{?source:String, ?originalLine:Int}>;

	@:ifFeature("haxe.CallStack.exceptionStack")
	static var lastExceptionTrace:NativeTrace;

	inline static function callStackImpl():Array<StackItem> {
		return makeStack(Global.debug_backtrace(Const.DEBUG_BACKTRACE_IGNORE_ARGS));
	}

	inline static function exceptionStackImpl():Array<StackItem> {
		return makeStack(lastExceptionTrace == null ? new NativeIndexedArray() : lastExceptionTrace);
	}

	static function complementTrace(nativeTrace:NativeTrace, e:Throwable):NativeTrace {
		var thrownAt = new NativeAssocArray<Dynamic>();
		thrownAt['function'] = '';
		thrownAt['line'] = e.getLine();
		thrownAt['file'] = e.getFile();
		thrownAt['class'] = '';
		thrownAt['args'] = new NativeArray();
		Global.array_unshift(nativeTrace, thrownAt);
		return nativeTrace;
	}

	static function makeStack(native:NativeTrace):Array<StackItem> {
		var result = [];
		var count = Global.count(native);

		for (i in 0...count) {
			var entry = native[i];
			var item = null;

			if (i + 1 < count) {
				var next = native[i + 1];

				if (!Global.isset(next['function']))
					next['function'] = '';
				if (!Global.isset(next['class']))
					next['class'] = '';

				if ((next['function'] : String).indexOf('{closure}') >= 0) {
					item = LocalFunction();
				} else if (Global.strlen(next['class']) > 0 && Global.strlen(next['function']) > 0) {
					var cls = Boot.getClassName(next['class']);
					item = Method(cls, next['function']);
				}
			}
			if (Global.isset(entry['file'])) {
				if (mapPosition != null) {
					var pos = mapPosition(entry['file'], entry['line']);
					if (pos != null && pos.source != null && pos.originalLine != null) {
						entry['file'] = pos.source;
						entry['line'] = pos.originalLine;
					}
				}
				result.push(FilePos(item, entry['file'], entry['line']));
			} else if (item != null) {
				result.push(item);
			}
		}

		return result;
	}

// end of PHP implementation
#else

	/**
		This method is used internally by some targets for non-haxe.Exception catches
		to provide stack for `haxe.CallStack.exceptionStack()`
	**/
	static inline function saveExceptionStack(e:Any) {
		#if js
			lastException = e;
		#elseif cs
			cs.internal.Exceptions.exception = e;
		#elseif java
			@:privateAccess inline java.internal.Exceptions.setException(e);
		#end
	}

	#if js
	static var lastException:js.lib.Error;

	static function getStack(e:js.lib.Error):Array<StackItem> {
		if (e == null)
			return [];
		// https://v8.dev/docs/stack-trace-api
		var oldValue = V8Error.prepareStackTrace;
		V8Error.prepareStackTrace = function(error, callsites) {
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
		var a = makeStack(e.stack);
		V8Error.prepareStackTrace = oldValue;
		return a;
	}

	// support for source-map-support module
	@:noCompletion
	public static var wrapCallSite:V8CallSite->V8CallSite;
	#end

	#if eval
	static function getCallStack() {
		return [];
	}

	static function getExceptionStack() {
		return [];
	}
	#end

	inline static function callStackImpl():Array<StackItem> {
		#if neko
		var a = makeStack(untyped __dollar__callstack());
		a.shift(); // remove Stack.callStack()
		return a;
		#elseif flash
		var a = makeStack(new flash.errors.Error().getStackTrace());
		a.shift(); // remove Stack.callStack()
		return a;
		#elseif cpp
		var s:Array<String> = untyped __global__.__hxcpp_get_call_stack(true);
		return makeStack(s);
		#elseif js
		try {
			throw new js.lib.Error();
		} catch (e:Dynamic) {
			var a = getStack(js.Lib.getOriginalException());
			a.shift(); // remove Stack.callStack()
			return a;
		}
		#elseif java
		var stack = makeStack(java.lang.Thread.currentThread().getStackTrace());
		stack.shift();
		stack.shift();
		stack.pop();
		return stack;
		#elseif cs
		return makeStack(new cs.system.diagnostics.StackTrace(1, true));
		#elseif python
		var stack = [];
		var infos = python.lib.Traceback.extract_stack();
		infos.pop();
		infos.reverse();
		for (elem in infos)
			stack.push(FilePos(Method(null, elem._3), elem._1, elem._2));
		return stack;
		#elseif lua
		var stack = [];
		var infos = lua.Debug.traceback();
		var luastack = infos.split("\n").slice(2, -1);
		for (s in luastack) {
			var parts = s.split(":");
			var file = parts[0];
			var line = parts[1];
			var method = if(parts.length <= 2) {
				null;
			} else {
				var methodPos = parts[2].indexOf("'");
				if(methodPos < 0) {
					null;
				} else {
					Method(null, parts[2].substring(methodPos + 1, parts[2].length - 1));
				}
			}
			stack.push(FilePos(method, file, Std.parseInt(line)));
		}
		return stack;
		#elseif hl
		try {
			throw null;
		} catch (e:Dynamic) {
			var st = _getExceptionStack();
			return makeStack(st.length > 2 ? st.sub(2, st.length - 2) : st);
		}
		#elseif eval
		return getCallStack();
		#else
		return []; // Unsupported
		#end
	}

	#if hl
	@:hlNative("std", "exception_stack") static function _getExceptionStack():hl.NativeArray<hl.Bytes> {
		return null;
	}
	#end

	inline static function exceptionStackImpl():Array<StackItem> {
		#if neko
		return makeStack(untyped __dollar__excstack());
		#elseif hl
		return makeStack(_getExceptionStack());
		#elseif flash
		var err:flash.errors.Error = untyped flash.Boot.lastError;
		if (err == null)
			return new Array();
		var a = makeStack(err.getStackTrace());
		var c = callStack();
		var i = c.length - 1;
		while (i > 0) {
			if (Std.string(a[a.length - 1]) == Std.string(c[i]))
				a.pop();
			else
				break;
			i--;
		}
		return a;
		#elseif cpp
		var s:Array<String> = untyped __global__.__hxcpp_get_exception_stack();
		return makeStack(s);
		#elseif java
		switch (#if jvm jvm.Exception #else java.internal.Exceptions #end.currentException()) {
			case null:
				return [];
			case current:
				return makeStack(current.getStackTrace());
		}
		#elseif cs
		return cs.internal.Exceptions.exception == null ? [] : makeStack(new cs.system.diagnostics.StackTrace(cs.internal.Exceptions.exception, true));
		#elseif python
		var stack = [];
		var exc = python.lib.Sys.exc_info();
		if (exc._3 != null) {
			var infos = python.lib.Traceback.extract_tb(exc._3);
			infos.reverse();
			for (elem in infos)
				stack.push(FilePos(Method(null, elem._3), elem._1, elem._2));
		}
		return stack;
		#elseif js
		return getStack(lastException);
		#elseif eval
		return getExceptionStack();
		#else
		return []; // Unsupported
		#end
	}

	#if cpp
	@:noDebug /* Do not mess up the exception stack */
	#end
	private static function makeStack(s:NativeTrace) {
		#if neko
		var a = new Array();
		var l = untyped __dollar__asize(s);
		var i = 0;
		while (i < l) {
			var x = s[i++];
			if (x == null)
				a.unshift(CFunction);
			else if (untyped __dollar__typeof(x) == __dollar__tstring)
				a.unshift(Module(new String(x)));
			else
				a.unshift(FilePos(null, new String(untyped x[0]), untyped x[1]));
		}
		return a;
		#elseif flash
		var a = new Array();
		var r = ~/at ([^\/]+?)\$?(\/[^\(]+)?\(\)(\[(.*?):([0-9]+)\])?/;
		var rlambda = ~/^MethodInfo-([0-9]+)$/g;
		while (r.match(s)) {
			var cl = r.matched(1).split("::").join(".");
			var meth = r.matched(2);
			var item;
			if (meth == null) {
				if (rlambda.match(cl))
					item = LocalFunction(Std.parseInt(rlambda.matched(1)));
				else
					item = Method(cl, "new");
			} else
				item = Method(cl, meth.substr(1));
			if (r.matched(3) != null)
				item = FilePos(item, r.matched(4), Std.parseInt(r.matched(5)));
			a.push(item);
			s = r.matchedRight();
		}
		return a;
		#elseif cpp
		var stack:Array<String> = s;
		var m = new Array<StackItem>();
		for (func in stack) {
			var words = func.split("::");
			if (words.length == 0)
				m.push(CFunction)
			else if (words.length == 2)
				m.push(Method(words[0], words[1]));
			else if (words.length == 4)
				m.push(FilePos(Method(words[0], words[1]), words[2], Std.parseInt(words[3])));
		}
		return m;
		#elseif js
		if (s == null) {
			return [];
		} else if (js.Syntax.typeof(s) == "string") {
			// Return the raw lines in browsers that don't support prepareStackTrace
			var stack:Array<String> = s.split("\n");
			if (stack[0] == "Error")
				stack.shift();
			var m = [];
			var rie10 = ~/^    at ([A-Za-z0-9_. ]+) \(([^)]+):([0-9]+):([0-9]+)\)$/;
			for (line in stack) {
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
		} else {
			return cast s;
		}
		#elseif cs
		var stack = [];
		for (i in 0...s.FrameCount) {
			var frame = s.GetFrame(i);
			var m = frame.GetMethod();

			if (m == null) {
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
		#elseif java
		var stack = [];
		for (el in s) {
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
		#elseif hl
		var stack = [];
		var r = ~/^([A-Za-z0-9.$_]+)\.([~A-Za-z0-9_]+(\.[0-9]+)?)\((.+):([0-9]+)\)$/;
		var r_fun = ~/^fun\$([0-9]+)\((.+):([0-9]+)\)$/;
		for (i in 0...s.length - 1) {
			var str = @:privateAccess String.fromUCS2(s[i]);
			if (r.match(str))
				stack.push(FilePos(Method(r.matched(1), r.matched(2)), r.matched(4), Std.parseInt(r.matched(5))));
			else if (r_fun.match(str))
				stack.push(FilePos(LocalFunction(Std.parseInt(r_fun.matched(1))), r_fun.matched(2), Std.parseInt(r_fun.matched(3))));
			else
				stack.push(Module(str));
		}
		return stack;
		#else
		return null;
		#end
	}
#end
}

#if js
// https://v8.dev/docs/stack-trace-api
@:native("Error")
private extern class V8Error {
	static var prepareStackTrace:(error:js.lib.Error, structuredStackTrace:Array<V8CallSite>)->Any;
}

typedef V8CallSite = {
	function getFunctionName():String;
	function getFileName():String;
	function getLineNumber():Int;
	function getColumnNumber():Int;
}
#end