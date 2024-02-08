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

package js.lib;

import haxe.extern.Rest;

@:native("Function")
extern class Function {
	/** Specifies the number of arguments expected by the function. **/
	var length(default, never):Int;

	/** The name of the function. **/
	var name:String;

	/** Creates a new Function object. **/
	function new(arg:String, rest:Rest<String>);

	/** Calls a function and sets its this to the provided value, arguments can be passed as an Array object. **/
	function apply(thisArg:Dynamic, argsArray:Array<Dynamic>):Dynamic;

	/** Calls (executes) a function and sets its this to the provided value, arguments can be passed as they are. **/
	function call(thisArg:Dynamic, args:Rest<Dynamic>):Dynamic;

	/**
		Creates a new function which, when called, has its this set to the provided value,
		with a given sequence of arguments preceding any provided when the new function was called.
	**/
	@:pure function bind(thisArg:Dynamic, args:Rest<Dynamic>):Function;

	/** Returns a string representing the source code of the function. **/
	@:pure function toString():String;
}
