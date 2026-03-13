/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node.vm;

import js.node.Vm.VmContext;

typedef ScriptOptions = {
	/**
		The filename that shows up in any stack traces produced.
	**/
	@:optional var filename:String;

	/**
		Whether or not to print any errors to stderr, with the line of code that caused them highlighted,
		before throwing an exception.

		Will capture both syntax errors from compiling code and runtime errors thrown by executing the compiled code.

		Defaults to true.
	**/
	@:optional var displayErrors:Bool;
}

typedef ScriptRunOptions = {
	/**
		Whether or not to print any errors to stderr, with the line of code that caused them highlighted,
		before throwing an exception.

		Will capture both syntax errors from compiling code and runtime errors thrown by executing the compiled code.

		Defaults to true.
	**/
	@:optional var displayErrors:Bool;

	/**
		Number of milliseconds to execute code before terminating execution.
		If execution is terminated, an Error will be thrown.
	**/
	@:optional var timeout:Int;
}

/**
	A class for holding precompiled scripts, and running them in specific sandboxes.
**/
@:jsRequire("vm", "Script")
extern class Script {
	/**
		Creating a new `Script` compiles `code` but does not run it. Instead, the created `Script` object
		represents this compiled code.

		This script can be run later many times using methods below.

		The returned script is not bound to any global object. It is bound before each run, just for that run.
	**/
	function new(code:String, ?options:ScriptOptions);

	/**
		Similar to `Vm.runInThisContext` but a method of a precompiled `Script` object.
		`runInThisContext` runs the code of script and returns the result.
		Running code does not have access to local scope, but does have access to the current global object.
	**/
	function runInThisContext(?options:ScriptRunOptions):Dynamic;

	/**
		Similar to `Vm.runInContext` but a method of a precompiled `Script` object.
		`runInContext` runs script's compiled code in `contextifiedSandbox` and returns the result.
		Running code does not have access to local scope.
	**/
	function runInContext(contextifiedSandbox:VmContext<Dynamic>, ?options:ScriptRunOptions):Dynamic;

	/**
		Similar to `Vm.runInNewContext` but a method of a precompiled `Script` object.
		`runInNewContext` contextifies sandbox if passed or creates a new contextified sandbox if it's omitted,
		and then runs script's compiled code with the sandbox as the global object and returns the result.
		Running code does not have access to local scope.
	**/
	@:overload(function(sandbox:{}, ?options:ScriptRunOptions):Dynamic {})
	function runInNewContext(?sandbox:{}):Dynamic;
}
