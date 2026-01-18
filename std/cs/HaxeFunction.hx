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
 * Dual-slot invoke pattern: Each argument has two slots - a double slot for primitives
 * (int, float, bool) and an object slot for references (and long for precision).
 * When object slot == Runtime.undefined, use the double slot.
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
	// Dual-slot invoke methods - these avoid boxing for primitives
	// __hx_invokeN_o returns object, __hx_invokeN_f returns double
	// ============================================================

	public function __hx_invoke0_o():Dynamic {
		return invokeDynamic([]);
	}

	public function __hx_invoke0_f():Float {
		return __hx_invoke0_o();
	}

	public function __hx_invoke1_o(f1:Float, d1:Dynamic):Dynamic {
		return invokeDynamic([d1]);
	}

	public function __hx_invoke1_f(f1:Float, d1:Dynamic):Float {
		return __hx_invoke1_o(f1, d1);
	}

	public function __hx_invoke2_o(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic):Dynamic {
		return invokeDynamic([d1, d2]);
	}

	public function __hx_invoke2_f(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic):Float {
		return __hx_invoke2_o(f1, d1, f2, d2);
	}

	public function __hx_invoke3_o(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic):Dynamic {
		return invokeDynamic([d1, d2, d3]);
	}

	public function __hx_invoke3_f(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic):Float {
		return __hx_invoke3_o(f1, d1, f2, d2, f3, d3);
	}

	public function __hx_invoke4_o(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic):Dynamic {
		return invokeDynamic([d1, d2, d3, d4]);
	}

	public function __hx_invoke4_f(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic):Float {
		return __hx_invoke4_o(f1, d1, f2, d2, f3, d3, f4, d4);
	}

	public function __hx_invoke5_o(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic, f5:Float, d5:Dynamic):Dynamic {
		return invokeDynamic([d1, d2, d3, d4, d5]);
	}

	public function __hx_invoke5_f(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic, f5:Float, d5:Dynamic):Float {
		return __hx_invoke5_o(f1, d1, f2, d2, f3, d3, f4, d4, f5, d5);
	}

	public function __hx_invoke6_o(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic, f5:Float, d5:Dynamic, f6:Float,
			d6:Dynamic):Dynamic {
		return invokeDynamic([d1, d2, d3, d4, d5, d6]);
	}

	public function __hx_invoke6_f(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic, f5:Float, d5:Dynamic, f6:Float,
			d6:Dynamic):Float {
		return __hx_invoke6_o(f1, d1, f2, d2, f3, d3, f4, d4, f5, d5, f6, d6);
	}

	public function __hx_invoke7_o(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic, f5:Float, d5:Dynamic, f6:Float,
			d6:Dynamic, f7:Float, d7:Dynamic):Dynamic {
		return invokeDynamic([d1, d2, d3, d4, d5, d6, d7]);
	}

	public function __hx_invoke7_f(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic, f5:Float, d5:Dynamic, f6:Float,
			d6:Dynamic, f7:Float, d7:Dynamic):Float {
		return __hx_invoke7_o(f1, d1, f2, d2, f3, d3, f4, d4, f5, d5, f6, d6, f7, d7);
	}

	public function __hx_invoke8_o(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic, f5:Float, d5:Dynamic, f6:Float,
			d6:Dynamic, f7:Float, d7:Dynamic, f8:Float, d8:Dynamic):Dynamic {
		return invokeDynamic([d1, d2, d3, d4, d5, d6, d7, d8]);
	}

	public function __hx_invoke8_f(f1:Float, d1:Dynamic, f2:Float, d2:Dynamic, f3:Float, d3:Dynamic, f4:Float, d4:Dynamic, f5:Float, d5:Dynamic, f6:Float,
			d6:Dynamic, f7:Float, d7:Dynamic, f8:Float, d8:Dynamic):Float {
		return __hx_invoke8_o(f1, d1, f2, d2, f3, d3, f4, d4, f5, d5, f6, d6, f7, d7, f8, d8);
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
		return __hx_invoke1_o(0.0, a0);
	}

	/**
	 * Invoke with 2 arguments.
	 */
	public function invoke2(a0:Dynamic, a1:Dynamic):Dynamic {
		return __hx_invoke2_o(0.0, a0, 0.0, a1);
	}

	/**
	 * Invoke with 3 arguments.
	 */
	public function invoke3(a0:Dynamic, a1:Dynamic, a2:Dynamic):Dynamic {
		return __hx_invoke3_o(0.0, a0, 0.0, a1, 0.0, a2);
	}

	/**
	 * Invoke with 4 arguments.
	 */
	public function invoke4(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic):Dynamic {
		return __hx_invoke4_o(0.0, a0, 0.0, a1, 0.0, a2, 0.0, a3);
	}

	/**
	 * Invoke with 5 arguments.
	 */
	public function invoke5(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic):Dynamic {
		return __hx_invoke5_o(0.0, a0, 0.0, a1, 0.0, a2, 0.0, a3, 0.0, a4);
	}

	/**
	 * Invoke with 6 arguments.
	 */
	public function invoke6(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic):Dynamic {
		return __hx_invoke6_o(0.0, a0, 0.0, a1, 0.0, a2, 0.0, a3, 0.0, a4, 0.0, a5);
	}

	/**
	 * Invoke with 7 arguments.
	 */
	public function invoke7(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic):Dynamic {
		return __hx_invoke7_o(0.0, a0, 0.0, a1, 0.0, a2, 0.0, a3, 0.0, a4, 0.0, a5, 0.0, a6);
	}

	/**
	 * Invoke with 8 arguments.
	 */
	public function invoke8(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic, a7:Dynamic):Dynamic {
		return __hx_invoke8_o(0.0, a0, 0.0, a1, 0.0, a2, 0.0, a3, 0.0, a4, 0.0, a5, 0.0, a6, 0.0, a7);
	}
}
