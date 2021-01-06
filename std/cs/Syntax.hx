/*
 * Copyright (C)2005-2021 Haxe Foundation
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

import haxe.extern.Rest;

/**
	Generate C# syntax not directly supported by Haxe.
	Use only at low-level when specific target-specific code-generation is required.
**/
@:noClosure
extern class Syntax {
	/**
		Inject `code` directly into generated source.

		`code` must be a string constant.

		Additional `args` are supported to provide code interpolation, for example:
		```haxe
		Syntax.code("System.Console.WriteLine({0}, {1})", "hi", 42);
		```
		will generate
		```haxe
		System.Console.WriteLine("hi", 42);
		```

		Emits a compilation error if the count of `args` does not match the count of placeholders in `code`.
	**/
	static function code(code:String, args:Rest<Dynamic>):Dynamic;

	/**
		Inject `code` directly into generated source.
		The same as `cs.Syntax.code` except this one does not provide code interpolation.
	**/
	static function plainCode(code:String):Dynamic;
}
