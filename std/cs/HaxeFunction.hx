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

	/**
	 * Invoke with 0 arguments.
	 */
	public function invoke():Dynamic {
		return invokeDynamic([]);
	}

	/**
	 * Invoke with 1 argument.
	 */
	public function invoke1(a0:Dynamic):Dynamic {
		return invokeDynamic([a0]);
	}

	/**
	 * Invoke with 2 arguments.
	 */
	public function invoke2(a0:Dynamic, a1:Dynamic):Dynamic {
		return invokeDynamic([a0, a1]);
	}

	/**
	 * Invoke with 3 arguments.
	 */
	public function invoke3(a0:Dynamic, a1:Dynamic, a2:Dynamic):Dynamic {
		return invokeDynamic([a0, a1, a2]);
	}

	/**
	 * Invoke with 4 arguments.
	 */
	public function invoke4(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic):Dynamic {
		return invokeDynamic([a0, a1, a2, a3]);
	}

	/**
	 * Invoke with 5 arguments.
	 */
	public function invoke5(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic):Dynamic {
		return invokeDynamic([a0, a1, a2, a3, a4]);
	}

	/**
	 * Invoke with 6 arguments.
	 */
	public function invoke6(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic):Dynamic {
		return invokeDynamic([a0, a1, a2, a3, a4, a5]);
	}

	/**
	 * Invoke with 7 arguments.
	 */
	public function invoke7(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic):Dynamic {
		return invokeDynamic([a0, a1, a2, a3, a4, a5, a6]);
	}

	/**
	 * Invoke with 8 arguments.
	 */
	public function invoke8(a0:Dynamic, a1:Dynamic, a2:Dynamic, a3:Dynamic, a4:Dynamic, a5:Dynamic, a6:Dynamic, a7:Dynamic):Dynamic {
		return invokeDynamic([a0, a1, a2, a3, a4, a5, a6, a7]);
	}
}
