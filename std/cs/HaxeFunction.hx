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

import cs.Int64;

/**
 * Base class for all Haxe function types in C#.
 * Provides dynamic invocation capability for calling functions via reflection.
 *
 * FunctionArg pattern: Each argument is passed as a FunctionArg struct that can hold
 * either primitives (via prim field) or references (via obj field) without boxing.
 * The hasValue field tracks whether the argument was provided (for optional parameters).
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
	// FunctionArg-based invoke methods - zero allocation!
	// __hx_invokeN_o returns object, __hx_invokeN_l returns long
	// ============================================================

	public function __hx_invoke0_o():Dynamic {
		return invokeDynamic([]);
	}

	public function __hx_invoke0_l():Int64 {
		return __hx_invoke0_o();
	}

	public function __hx_invoke1_o(a1:FunctionArg):Dynamic {
		return invokeDynamic([a1.ToDynamic()]);
	}

	public function __hx_invoke1_l(a1:FunctionArg):Int64 {
		return __hx_invoke1_o(a1);
	}

	public function __hx_invoke2_o(a1:FunctionArg, a2:FunctionArg):Dynamic {
		return invokeDynamic([a1.ToDynamic(), a2.ToDynamic()]);
	}

	public function __hx_invoke2_l(a1:FunctionArg, a2:FunctionArg):Int64 {
		return __hx_invoke2_o(a1, a2);
	}

	public function __hx_invoke3_o(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg):Dynamic {
		return invokeDynamic([a1.ToDynamic(), a2.ToDynamic(), a3.ToDynamic()]);
	}

	public function __hx_invoke3_l(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg):Int64 {
		return __hx_invoke3_o(a1, a2, a3);
	}

	public function __hx_invoke4_o(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg):Dynamic {
		return invokeDynamic([a1.ToDynamic(), a2.ToDynamic(), a3.ToDynamic(), a4.ToDynamic()]);
	}

	public function __hx_invoke4_l(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg):Int64 {
		return __hx_invoke4_o(a1, a2, a3, a4);
	}

	public function __hx_invoke5_o(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg, a5:FunctionArg):Dynamic {
		return invokeDynamic([a1.ToDynamic(), a2.ToDynamic(), a3.ToDynamic(), a4.ToDynamic(), a5.ToDynamic()]);
	}

	public function __hx_invoke5_l(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg, a5:FunctionArg):Int64 {
		return __hx_invoke5_o(a1, a2, a3, a4, a5);
	}

	public function __hx_invoke6_o(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg, a5:FunctionArg, a6:FunctionArg):Dynamic {
		return invokeDynamic([a1.ToDynamic(), a2.ToDynamic(), a3.ToDynamic(), a4.ToDynamic(), a5.ToDynamic(), a6.ToDynamic()]);
	}

	public function __hx_invoke6_l(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg, a5:FunctionArg, a6:FunctionArg):Int64 {
		return __hx_invoke6_o(a1, a2, a3, a4, a5, a6);
	}

	public function __hx_invoke7_o(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg, a5:FunctionArg, a6:FunctionArg, a7:FunctionArg):Dynamic {
		return invokeDynamic([a1.ToDynamic(), a2.ToDynamic(), a3.ToDynamic(), a4.ToDynamic(), a5.ToDynamic(), a6.ToDynamic(), a7.ToDynamic()]);
	}

	public function __hx_invoke7_l(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg, a5:FunctionArg, a6:FunctionArg, a7:FunctionArg):Int64 {
		return __hx_invoke7_o(a1, a2, a3, a4, a5, a6, a7);
	}

	public function __hx_invoke8_o(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg, a5:FunctionArg, a6:FunctionArg, a7:FunctionArg,
			a8:FunctionArg):Dynamic {
		return invokeDynamic([
			a1.ToDynamic(),
			a2.ToDynamic(),
			a3.ToDynamic(),
			a4.ToDynamic(),
			a5.ToDynamic(),
			a6.ToDynamic(),
			a7.ToDynamic(),
			a8.ToDynamic()
		]);
	}

	public function __hx_invoke8_l(a1:FunctionArg, a2:FunctionArg, a3:FunctionArg, a4:FunctionArg, a5:FunctionArg, a6:FunctionArg, a7:FunctionArg,
			a8:FunctionArg):Int64 {
		return __hx_invoke8_o(a1, a2, a3, a4, a5, a6, a7, a8);
	}

	// ============================================================
	// Convenience invoke methods - these box all arguments
	// ============================================================

	/**
	 * Invoke with 0 arguments.
	 */
	public function invoke():Dynamic {
		return __hx_invoke0_o();
	}

	/**
	 * Invoke with 1 argument.
	 */
	public function invoke1(a0:Dynamic):Dynamic {
		return __hx_invoke1_o(FunctionArg.FromObject(a0));
	}

	/**
	 * Invoke with 2 arguments.
	 */
	public function invoke2(a0:Dynamic, a1:Dynamic):Dynamic {
		return __hx_invoke2_o(FunctionArg.FromObject(a0), FunctionArg.FromObject(a1));
	}

	/**
	 * Invoke with 3 arguments.
	 */
	public function invoke3(a0:Dynamic, a1:Dynamic, a2:Dynamic):Dynamic {
		return __hx_invoke3_o(FunctionArg.FromObject(a0), FunctionArg.FromObject(a1), FunctionArg.FromObject(a2));
	}

	/**
	 * Invoke with 4 arguments.
	 */
	public function invoke4(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic):Dynamic {
		return __hx_invoke4_o(FunctionArg.FromObject(a0), FunctionArg.FromObject(a1), FunctionArg.FromObject(a2), FunctionArg.FromObject(a3));
	}

	/**
	 * Invoke with 5 arguments.
	 */
	public function invoke5(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic):Dynamic {
		return __hx_invoke5_o(FunctionArg.FromObject(a0), FunctionArg.FromObject(a1), FunctionArg.FromObject(a2), FunctionArg.FromObject(a3),
			FunctionArg.FromObject(a4));
	}

	/**
	 * Invoke with 6 arguments.
	 */
	public function invoke6(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic):Dynamic {
		return __hx_invoke6_o(FunctionArg.FromObject(a0), FunctionArg.FromObject(a1), FunctionArg.FromObject(a2), FunctionArg.FromObject(a3),
			FunctionArg.FromObject(a4), FunctionArg.FromObject(a5));
	}

	/**
	 * Invoke with 7 arguments.
	 */
	public function invoke7(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic):Dynamic {
		return __hx_invoke7_o(FunctionArg.FromObject(a0), FunctionArg.FromObject(a1), FunctionArg.FromObject(a2), FunctionArg.FromObject(a3),
			FunctionArg.FromObject(a4), FunctionArg.FromObject(a5), FunctionArg.FromObject(a6));
	}

	/**
	 * Invoke with 8 arguments.
	 */
	public function invoke8(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic, a7:Dynamic):Dynamic {
		return __hx_invoke8_o(FunctionArg.FromObject(a0), FunctionArg.FromObject(a1), FunctionArg.FromObject(a2), FunctionArg.FromObject(a3),
			FunctionArg.FromObject(a4), FunctionArg.FromObject(a5), FunctionArg.FromObject(a6), FunctionArg.FromObject(a7));
	}
}
