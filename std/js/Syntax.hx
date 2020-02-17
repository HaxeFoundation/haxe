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

package js;

import haxe.extern.Rest;

/**
	Generate JavaScript syntax not directly supported by Haxe.
	Use only at low-level when specific target-specific code-generation is required.
**/
@:noClosure
extern class Syntax {
	/**
		Inject `code` directly into generated source.

		`code` must be a string constant.

		Additional `args` are supported to provide code interpolation, for example:
		```haxe
		Syntax.code("console.log({0}, {1})", "hi", 42);
		```
		will generate
		```haxe
		console.log("hi", 42);
		```

		Emits a compilation error if the count of `args` does not match the count of placeholders in `code`.
	**/
	static function code(code:String, args:Rest<Dynamic>):Dynamic;

	/**
		Inject `code` directly into generated source.
		The same as `js.Syntax.code` except this one does not provide code interpolation.
	**/
	static function plainCode(code:String):Dynamic;

	/**
		Generate `new cl(...args)` expression.
	**/
	@:overload(function(cl:String, args:Rest<Dynamic>):Dynamic {})
	static function construct<T>(cl:Class<T>, args:Rest<Dynamic>):T;

	/**
		Generate `v instanceof cl` expression.
	**/
	@:pure static function instanceof(v:Dynamic, cl:Class<Dynamic>):Bool;

	/**
		Generate `typeof o` expression.
	**/
	@:pure static function typeof(o:Dynamic):String;

	/**
		Genearte `a === b` expression.
	**/
	@:pure static function strictEq(a:Dynamic, b:Dynamic):Bool;

	/**
		Genearte `a !== b` expression.
	**/
	@:pure static function strictNeq(a:Dynamic, b:Dynamic):Bool;

	/**
		Generate `delete o[f]` expression.
	**/
	@:overload(function(o:Dynamic, f:Int):Bool {})
	static function delete(o:Dynamic, f:String):Bool;

	/**
		Generate `o.f` expression, if `f` is a constant string,
		or `o[f]` if it's any other expression.
	**/
	static function field(o:Dynamic, f:String):Dynamic;
}
