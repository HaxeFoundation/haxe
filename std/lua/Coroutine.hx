/*
 * Copyright (C)2005-2016 Haxe Foundation
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

package lua;

import haxe.Constraints.Function;
import haxe.extern.Rest;

/**
	Externs for native Lua coroutines.
**/
@:native("_G.coroutine")
extern class Coroutine {
	/**
		Creates a new coroutine, with body `f`. `f` must be a Lua function. 
	**/
	public static function create(f : Function)  : Thread;

	/**
		Returns the status of coroutine.
	**/
	public static function status(c : Coroutine) : ThreadState;

	/**
		Starts or continues the execution of coroutine. 
		The first time you resume a coroutine, it starts running its body.
		The values `args` are passed as the arguments to the body function.
		If the coroutine has yielded, `resume` restarts it; 
		the values `args` are passed as the results from the yield.

		If the coroutine runs without any errors, `resume` returns `true` plus any 
		values passed to `yield` (if the coroutine yields) or any values returned 
		by the body function (if the coroutine terminates). If there is any error, 
		`resume` returns `false` plus the error message.
	**/
	public static function resume(c : Coroutine, args : Rest<Dynamic>) : Dynamic;

	/**
		Suspends the execution of the calling coroutine. 
		The coroutine cannot be running a C function, a metamethod, or an iterator.
		Any arguments to `yield` are passed as extra results to `resume`.
	**/
	public static function yield(args : Rest<Dynamic>) : Dynamic;

	/**
		Creates a new coroutine, with body `f`.
		Returns a function that resumes the coroutine each time it is called. 
		Any arguments passed to the function behave as the extra arguments to `resume`. 
		Returns the same values returned by `resume`, except the first boolean. 
		In case of error, propagates the error.
	**/
	public static function wrap(f : Function) : Thread;
}

/**
	A enumerator that describes the output of `Coroutine.status()`.
**/
@:enum
abstract ThreadState(String) {
	/**
		If the coroutine is suspended in a call to yield, or if it has not started 
		running yet.
	**/
	var Suspended = "suspended";

	/**
		If the coroutine is running.
	**/
	var Running = "running";

	/**
		If the coroutine is active but not running. That is, it has resumed another 
		coroutine.
	**/
	var Normal = "normal";

	/**
		If the coroutine has finished its body function or if it has stopped with 
		an error.
	**/
	var Dead = "dead";
}

