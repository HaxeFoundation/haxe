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
 * FunctionValue pattern: Each argument and return value is passed as a FunctionValue struct
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
	// FunctionValue-based invoke methods - zero allocation!
	// Returns FunctionValue to avoid boxing return values too.
	// ============================================================

	public function __hx_invoke0():FunctionValue {
		return FunctionValue.FromObject(invokeDynamic([]));
	}

	public function __hx_invoke1(a1:FunctionValue):FunctionValue {
		return FunctionValue.FromObject(invokeDynamic([a1.ToDynamic()]));
	}

	public function __hx_invoke2(a1:FunctionValue, a2:FunctionValue):FunctionValue {
		return FunctionValue.FromObject(invokeDynamic([a1.ToDynamic(), a2.ToDynamic()]));
	}

	public function __hx_invoke3(a1:FunctionValue, a2:FunctionValue, a3:FunctionValue):FunctionValue {
		return FunctionValue.FromObject(invokeDynamic([a1.ToDynamic(), a2.ToDynamic(), a3.ToDynamic()]));
	}

	public function __hx_invoke4(a1:FunctionValue, a2:FunctionValue, a3:FunctionValue, a4:FunctionValue):FunctionValue {
		return FunctionValue.FromObject(invokeDynamic([a1.ToDynamic(), a2.ToDynamic(), a3.ToDynamic(), a4.ToDynamic()]));
	}

	public function __hx_invoke5(a1:FunctionValue, a2:FunctionValue, a3:FunctionValue, a4:FunctionValue, a5:FunctionValue):FunctionValue {
		return FunctionValue.FromObject(invokeDynamic([a1.ToDynamic(), a2.ToDynamic(), a3.ToDynamic(), a4.ToDynamic(), a5.ToDynamic()]));
	}

	public function __hx_invoke6(a1:FunctionValue, a2:FunctionValue, a3:FunctionValue, a4:FunctionValue, a5:FunctionValue,
			a6:FunctionValue):FunctionValue {
		return FunctionValue.FromObject(invokeDynamic([
			a1.ToDynamic(),
			a2.ToDynamic(),
			a3.ToDynamic(),
			a4.ToDynamic(),
			a5.ToDynamic(),
			a6.ToDynamic()
		]));
	}

	public function __hx_invoke7(a1:FunctionValue, a2:FunctionValue, a3:FunctionValue, a4:FunctionValue, a5:FunctionValue, a6:FunctionValue,
			a7:FunctionValue):FunctionValue {
		return FunctionValue.FromObject(invokeDynamic([
			a1.ToDynamic(),
			a2.ToDynamic(),
			a3.ToDynamic(),
			a4.ToDynamic(),
			a5.ToDynamic(),
			a6.ToDynamic(),
			a7.ToDynamic()
		]));
	}

	public function __hx_invoke8(a1:FunctionValue, a2:FunctionValue, a3:FunctionValue, a4:FunctionValue, a5:FunctionValue, a6:FunctionValue,
			a7:FunctionValue, a8:FunctionValue):FunctionValue {
		return FunctionValue.FromObject(invokeDynamic([
			a1.ToDynamic(),
			a2.ToDynamic(),
			a3.ToDynamic(),
			a4.ToDynamic(),
			a5.ToDynamic(),
			a6.ToDynamic(),
			a7.ToDynamic(),
			a8.ToDynamic()
		]));
	}

	// ============================================================
	// Convenience invoke methods - these box all arguments
	// These call the FunctionValue methods with all args wrapped
	// Return object for compatibility with reflection/dynamic calls
	// ============================================================

	/**
	 * Invoke with 0 arguments.
	 */
	public function invoke():Dynamic {
		return __hx_invoke0().ToDynamic();
	}

	/**
	 * Invoke with 1 argument.
	 */
	public function invoke1(a0:Dynamic):Dynamic {
		return __hx_invoke1(FunctionValue.FromObject(a0)).ToDynamic();
	}

	/**
	 * Invoke with 2 arguments.
	 */
	public function invoke2(a0:Dynamic, a1:Dynamic):Dynamic {
		return __hx_invoke2(FunctionValue.FromObject(a0), FunctionValue.FromObject(a1)).ToDynamic();
	}

	/**
	 * Invoke with 3 arguments.
	 */
	public function invoke3(a0:Dynamic, a1:Dynamic, a2:Dynamic):Dynamic {
		return __hx_invoke3(FunctionValue.FromObject(a0), FunctionValue.FromObject(a1), FunctionValue.FromObject(a2)).ToDynamic();
	}

	/**
	 * Invoke with 4 arguments.
	 */
	public function invoke4(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic):Dynamic {
		return __hx_invoke4(FunctionValue.FromObject(a0), FunctionValue.FromObject(a1), FunctionValue.FromObject(a2), FunctionValue.FromObject(a3))
			.ToDynamic();
	}

	/**
	 * Invoke with 5 arguments.
	 */
	public function invoke5(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic):Dynamic {
		return __hx_invoke5(FunctionValue.FromObject(a0), FunctionValue.FromObject(a1), FunctionValue.FromObject(a2), FunctionValue.FromObject(a3),
			FunctionValue.FromObject(a4))
			.ToDynamic();
	}

	/**
	 * Invoke with 6 arguments.
	 */
	public function invoke6(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic):Dynamic {
		return __hx_invoke6(FunctionValue.FromObject(a0), FunctionValue.FromObject(a1), FunctionValue.FromObject(a2), FunctionValue.FromObject(a3),
			FunctionValue.FromObject(a4), FunctionValue.FromObject(a5))
			.ToDynamic();
	}

	/**
	 * Invoke with 7 arguments.
	 */
	public function invoke7(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic):Dynamic {
		return __hx_invoke7(FunctionValue.FromObject(a0), FunctionValue.FromObject(a1), FunctionValue.FromObject(a2), FunctionValue.FromObject(a3),
			FunctionValue.FromObject(a4), FunctionValue.FromObject(a5), FunctionValue.FromObject(a6))
			.ToDynamic();
	}

	/**
	 * Invoke with 8 arguments.
	 */
	public function invoke8(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic, a7:Dynamic):Dynamic {
		return __hx_invoke8(FunctionValue.FromObject(a0), FunctionValue.FromObject(a1), FunctionValue.FromObject(a2), FunctionValue.FromObject(a3),
			FunctionValue.FromObject(a4), FunctionValue.FromObject(a5), FunctionValue.FromObject(a6), FunctionValue.FromObject(a7))
			.ToDynamic();
	}
}
