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

/**
	Elements return by `CallStack` methods.
**/
enum StackItem {
	CFunction;
	Module(m:String);
	FilePos(s:Null<StackItem>, file:String, line:Int, ?column:Int);
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

	/**
		Return the call stack elements, or an empty array if not available.
	**/
	public static function callStack():Array<StackItem> {
		return NativeStackTrace.toHaxe(NativeStackTrace.callStack());
	}

	/**
		Return the exception stack : this is the stack elements between
		the place the last exception was thrown and the place it was
		caught, or an empty array if not available.
		Set `fullStack` parameter to true in order to return the full exception stack.

		May not work if catch type was a derivative from `haxe.Exception`.
	**/
	public static function exceptionStack( fullStack = false ):Array<StackItem> {
		var eStack:CallStack = NativeStackTrace.toHaxe(NativeStackTrace.exceptionStack());
		return (fullStack ? eStack : eStack.subtract(callStack())).asArray();
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

	/**
		Make a copy of the stack.
	**/
	public inline function copy():CallStack {
		return this.copy();
	}

	@:arrayAccess public inline function get(index:Int):StackItem {
		return this[index];
	}

	inline function asArray():Array<StackItem> {
		return this;
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

	static function exceptionToString(e:Exception):String {
		if(e.previous == null) {
			return 'Exception: ${e.toString()}${e.stack}';
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
}
