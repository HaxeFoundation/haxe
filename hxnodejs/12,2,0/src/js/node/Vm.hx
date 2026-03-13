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

package js.node;

import js.node.vm.Script;

/**
	Options object used by Vm.run* methods.
**/
typedef VmRunOptions = {
	> ScriptOptions,
	> ScriptRunOptions,
}

/**
	Using this class JavaScript code can be compiled and
	run immediately or compiled, saved, and run later.
**/
@:jsRequire("vm")
extern class Vm {
	/**
		Compiles `code`, runs it and returns the result.
		Running code does not have access to local scope.

		`filename` is optional, it's used only in stack traces.

		In case of syntax error in `code` emits the syntax error to stderr and throws an exception.
	**/
	static function runInThisContext(code:String, ?options:VmRunOptions):Dynamic;

	/**
		Compiles `code`, contextifies `sandbox` if passed or creates a new contextified sandbox if it's omitted,
		and then runs the code with the sandbox as the global object and returns the result.

		`runInNewContext` takes the same options as `runInThisContext`.

		Note that running untrusted code is a tricky business requiring great care. `runInNewContext` is quite useful,
		but safely running untrusted code requires a separate process.
	**/
	@:overload(function(code:String, ?sandbox:{}):Dynamic {})
	static function runInNewContext(code:String, sandbox:{}, ?options:VmRunOptions):Dynamic;

	/**
		Compiles `code`, then runs it in `contextifiedSandbox` and returns the result.

		Running code does not have access to local scope. The `contextifiedSandbox` object must have been previously
		contextified via `createContext`; it will be used as the global object for code.

		`runInContext` takes the same options as `runInThisContext`.

		Note that running untrusted code is a tricky business requiring great care. `runInContext` is quite useful,
		but safely running untrusted code requires a separate process.
	**/
	static function runInContext(code:String, contextifiedSandbox:VmContext<Dynamic>, ?options:VmRunOptions):Dynamic;

	/**

		If given a sandbox object, will "contextify" that sandbox so that it can be used in calls to `runInContext` or
		`Script.runInContext`. Inside scripts run as such, sandbox will be the global object, retaining all its existing
		properties but also having the built-in objects and functions any standard global object has. Outside of scripts
		run by the vm module, sandbox will be unchanged.

		If not given a sandbox object, returns a new, empty contextified sandbox object you can use.

		This function is useful for creating a sandbox that can be used to run multiple scripts, e.g. if you were
		emulating a web browser it could be used to create a single sandbox representing a window's global object,
		then run all <script> tags together inside that sandbox.
	**/
	static function createContext<T:{}>(?sandbox:T):VmContext<T>;

	/**
		Returns whether or not a sandbox object has been contextified by calling `createContext` on it.
	**/
	static function isContext(sandbox:{}):Bool;

	/**
		Compiles and executes `code` inside the V8 debug context.
		The primary use case is to get access to the V8 debug object:

		Note that the debug context and object are intrinsically tied to V8's debugger implementation
		and may change (or even get removed) without prior warning.
	**/
	static function runInDebugContext(code:String):Dynamic;

	@:deprecated("use new js.node.vm.Script(...) instead")
	static function createScript(code:String, ?options:ScriptOptions):Script;
}

/**
	Type of context objects returned by `Vm.createContext`.
**/
@:forward
abstract VmContext<T:{}>(T) from T to T {}
