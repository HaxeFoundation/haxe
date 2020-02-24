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

using StringTools;

private typedef NativeTrace =
	#if cs cs.system.diagnostics.StackTrace
	#elseif python Array<python.Tuple.Tuple4<String, Int, String, String>>
	#elseif lua Array<String>
	#elseif flash String
	#elseif hl hl.NativeArray<hl.Bytes>
	#elseif cpp Array<String>
	#else Dynamic
	#end;

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
		var b = new StringBuf();
		for (s in stack.asArray()) {
			b.add('\nCalled from ');
			itemToString(b, s);
		}
		return b.toString();
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
		return NativeStackTrace.toHaxe(NativeStackTrace.callStack(), 1);
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
		return NativeStackTrace.toHaxe(NativeStackTrace.exceptionStack());
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

	#if cs
	@:meta(System.ThreadStaticAttribute)
	static var exception:Null<cs.system.Exception>;
	#elseif lua
	static var exception:Null<String>;
	#end

	/**
		This method is used internally by some targets for non-haxe.Exception catches
		to provide stack for `haxe.CallStack.exceptionStack()`
	**/
	static inline function saveExceptionStack(e:Any) {
		#if cs
			exception = e;
		#end
	}

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
		#elseif cs
		return makeStack(new cs.system.diagnostics.StackTrace(1, true));
		#elseif python
		return makeStack(pythonCallStack());
		#elseif lua
		return switch lua.Debug.traceback() {
			case null: [];
			case s: makeStack(s.split('\n').slice(2));
		}
		#elseif hl
		var stack = _getCallStack();
		return makeStack(stack.length > 3 ? stack.sub(3, stack.length - 3) : stack);
		#else
		return []; // Unsupported
		#end
	}

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
		#elseif cs
		return switch exception { case null: []; case e: makeStack(new cs.system.diagnostics.StackTrace(e, true)); }
		#elseif python
		return makeStack(pythonExceptionStack());
		#elseif lua
		return switch exception {
			case null: [];
			case s: makeStack(s.split('\n'));
		}
		#else
		return []; // Unsupported
		#end
	}

	#if cpp
	@:noDebug /* Do not mess up the exception stack */
	#end
	private static function makeStack(s:NativeTrace):Array<StackItem> {
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
		#elseif python
		return [for (elem in s) FilePos(Method(null, elem._3), elem._1, elem._2)];
		#elseif lua
		var stack = [];
		for (item in s) {
			var parts = item.substr(1).split(":"); //`substr` to skip a tab at the beginning of a line
			var file = parts[0];
			if(file == '[C]') continue;
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
		#else
		return [];
		#end
	}

#if hl
	@:hlNative("std", "exception_stack") static function _getExceptionStack():NativeTrace {
		return null;
	}

	//TODO: implement inside of hashlink the same ways as `_getExceptionStack`
	static function _getCallStack():NativeTrace {
		var stack:NativeTrace = try {
			throw new Exception('', null, 'stack');
		} catch (e:Exception) {
			_getExceptionStack();
		}
		return stack;
	}

#elseif python
	static inline function pythonExceptionStack():NativeTrace {
		var exc = python.lib.Sys.exc_info();
		if (exc._3 != null) {
			var infos = python.lib.Traceback.extract_tb(exc._3);
			infos.reverse();
			return infos;
		} else {
			return [];
		}
	}

	static inline function pythonCallStack():NativeTrace {
		var infos = python.lib.Traceback.extract_stack();
		infos.pop();
		infos.reverse();
		return infos;
	}
#end
}