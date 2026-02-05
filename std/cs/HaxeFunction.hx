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

package cs;

/**
 * Base class for all Haxe function types in C#.
 * Provides dynamic invocation capability for calling functions via reflection.
 *
 * Value pattern: Each argument and return value is passed as a Value struct
 * that can hold either primitives (via prim field) or references (via obj field) without boxing.
 * The kind field indicates which slot contains the value (0=none, 1=obj, 2=prim).
 */
@:keep
@:native("haxe.lang.Function")
class HaxeFunction {
	/**
	 * Invoke this function dynamically with the given arguments.
	 * Subclasses (generated closures) override this to call the actual function.
	 */
	public function invokeDynamic(args:Array<Dynamic>):Dynamic {
		// Default implementation - should be overridden
		throw new haxe.exceptions.NotImplementedException();
	}

	// ============================================================
	// Value-based invoke methods - zero allocation!
	// Returns Value to avoid boxing return values too.
	// ============================================================

	public function __hx_invoke0():Value {
		return Value.fromObject(invokeDynamic([]));
	}

	public function __hx_invoke1(a1:Value):Value {
		return Value.fromObject(invokeDynamic([a1.toDynamic()]));
	}

	public function __hx_invoke2(a1:Value, a2:Value):Value {
		return Value.fromObject(invokeDynamic([a1.toDynamic(), a2.toDynamic()]));
	}

	public function __hx_invoke3(a1:Value, a2:Value, a3:Value):Value {
		return Value.fromObject(invokeDynamic([a1.toDynamic(), a2.toDynamic(), a3.toDynamic()]));
	}

	public function __hx_invoke4(a1:Value, a2:Value, a3:Value, a4:Value):Value {
		return Value.fromObject(invokeDynamic([a1.toDynamic(), a2.toDynamic(), a3.toDynamic(), a4.toDynamic()]));
	}

	public function __hx_invoke5(a1:Value, a2:Value, a3:Value, a4:Value, a5:Value):Value {
		return Value.fromObject(invokeDynamic([a1.toDynamic(), a2.toDynamic(), a3.toDynamic(), a4.toDynamic(), a5.toDynamic()]));
	}

	public function __hx_invoke6(a1:Value, a2:Value, a3:Value, a4:Value, a5:Value,
			a6:Value):Value {
		return Value.fromObject(invokeDynamic([
			a1.toDynamic(),
			a2.toDynamic(),
			a3.toDynamic(),
			a4.toDynamic(),
			a5.toDynamic(),
			a6.toDynamic()
		]));
	}

	public function __hx_invoke7(a1:Value, a2:Value, a3:Value, a4:Value, a5:Value, a6:Value,
			a7:Value):Value {
		return Value.fromObject(invokeDynamic([
			a1.toDynamic(),
			a2.toDynamic(),
			a3.toDynamic(),
			a4.toDynamic(),
			a5.toDynamic(),
			a6.toDynamic(),
			a7.toDynamic()
		]));
	}

	public function __hx_invoke8(a1:Value, a2:Value, a3:Value, a4:Value, a5:Value, a6:Value,
			a7:Value, a8:Value):Value {
		return Value.fromObject(invokeDynamic([
			a1.toDynamic(),
			a2.toDynamic(),
			a3.toDynamic(),
			a4.toDynamic(),
			a5.toDynamic(),
			a6.toDynamic(),
			a7.toDynamic(),
			a8.toDynamic()
		]));
	}

	public function __hx_invoke9(a1:Value, a2:Value, a3:Value, a4:Value, a5:Value, a6:Value,
			a7:Value, a8:Value, a9:Value):Value {
		return Value.fromObject(invokeDynamic([
			a1.toDynamic(),
			a2.toDynamic(),
			a3.toDynamic(),
			a4.toDynamic(),
			a5.toDynamic(),
			a6.toDynamic(),
			a7.toDynamic(),
			a8.toDynamic(),
			a9.toDynamic()
		]));
	}

	// ============================================================
	// Convenience invoke methods - these box all arguments
	// These call the Value methods with all args wrapped
	// Return object for compatibility with reflection/dynamic calls
	// ============================================================

	/**
	 * Invoke with 0 arguments.
	 */
	public function invoke():Dynamic {
		return __hx_invoke0().toDynamic();
	}

	/**
	 * Invoke with 1 argument.
	 */
	public function invoke1(a0:Dynamic):Dynamic {
		return __hx_invoke1(Value.fromObject(a0)).toDynamic();
	}

	/**
	 * Invoke with 2 arguments.
	 */
	public function invoke2(a0:Dynamic, a1:Dynamic):Dynamic {
		return __hx_invoke2(Value.fromObject(a0), Value.fromObject(a1)).toDynamic();
	}

	/**
	 * Invoke with 3 arguments.
	 */
	public function invoke3(a0:Dynamic, a1:Dynamic, a2:Dynamic):Dynamic {
		return __hx_invoke3(Value.fromObject(a0), Value.fromObject(a1), Value.fromObject(a2)).toDynamic();
	}

	/**
	 * Invoke with 4 arguments.
	 */
	public function invoke4(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic):Dynamic {
		return __hx_invoke4(Value.fromObject(a0), Value.fromObject(a1), Value.fromObject(a2), Value.fromObject(a3))
			.toDynamic();
	}

	/**
	 * Invoke with 5 arguments.
	 */
	public function invoke5(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic):Dynamic {
		return __hx_invoke5(Value.fromObject(a0), Value.fromObject(a1), Value.fromObject(a2), Value.fromObject(a3),
			Value.fromObject(a4))
			.toDynamic();
	}

	/**
	 * Invoke with 6 arguments.
	 */
	public function invoke6(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic):Dynamic {
		return __hx_invoke6(Value.fromObject(a0), Value.fromObject(a1), Value.fromObject(a2), Value.fromObject(a3),
			Value.fromObject(a4), Value.fromObject(a5))
			.toDynamic();
	}

	/**
	 * Invoke with 7 arguments.
	 */
	public function invoke7(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic):Dynamic {
		return __hx_invoke7(Value.fromObject(a0), Value.fromObject(a1), Value.fromObject(a2), Value.fromObject(a3),
			Value.fromObject(a4), Value.fromObject(a5), Value.fromObject(a6))
			.toDynamic();
	}

	/**
	 * Invoke with 8 arguments.
	 */
	public function invoke8(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic, a7:Dynamic):Dynamic {
		return __hx_invoke8(Value.fromObject(a0), Value.fromObject(a1), Value.fromObject(a2), Value.fromObject(a3),
			Value.fromObject(a4), Value.fromObject(a5), Value.fromObject(a6), Value.fromObject(a7))
			.toDynamic();
	}

	/**
	 * Invoke with 9 arguments.
	 */
	public function invoke9(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic, a7:Dynamic, a8:Dynamic):Dynamic {
		return __hx_invoke9(Value.fromObject(a0), Value.fromObject(a1), Value.fromObject(a2), Value.fromObject(a3),
			Value.fromObject(a4), Value.fromObject(a5), Value.fromObject(a6), Value.fromObject(a7),
			Value.fromObject(a8))
			.toDynamic();
	}
}
