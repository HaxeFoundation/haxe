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

import haxe.DynamicAccess;
import js.node.repl.REPLServer;
import js.node.stream.Readable.IReadable;
import js.node.stream.Writable.IWritable;
#if haxe4
import js.lib.Error;
import js.lib.Symbol;
#else
import js.Error;
#end

/**
	The `repl` module provides a Read-Eval-Print-Loop (REPL) implementation that is available both as a standalone
	program or includible in other applications.

	@see https://nodejs.org/api/repl.html#repl_repl
**/
@:jsRequire("repl")
extern class Repl {
	/**
		The `repl.start()` method creates and starts a `repl.REPLServer` instance.

		@see https://nodejs.org/api/repl.html#repl_repl_start_options
	**/
	@:overload(function(prompt:String):REPLServer {})
	static function start(options:ReplOptions):REPLServer;

	/**
		Evaluates expressions in sloppy mode.

		@see https://nodejs.org/api/repl.html#repl_repl_start_options
	**/
	#if haxe4
	static final REPL_MODE_SLOPPY:Symbol;
	#else
	static var REPL_MODE_SLOPPY(default, never):Dynamic;
	#end

	/**
		Evaluates expressions in strict mode.
		This is equivalent to prefacing every repl statement with `'use strict'`.

		@see https://nodejs.org/api/repl.html#repl_repl_start_options
	**/
	#if haxe4
	static final REPL_MODE_STRICT:Symbol;
	#else
	static var REPL_MODE_STRICT(default, never):Dynamic;
	#end
}

/**
	Options object used by `Repl.start`.

	@see https://nodejs.org/api/repl.html#repl_repl_start_options
**/
typedef ReplOptions = {
	/**
		The input prompt to display.

		Default: `'> '` (with a trailing space).
	**/
	@:optional var prompt:String;

	/**
		The `Readable` stream from which REPL input will be read.

		Default: `process.stdin`.
	**/
	@:optional var input:IReadable;

	/**
		The `Writable` stream to which REPL output will be written.

		Default: `process.stdout`.
	**/
	@:optional var output:IWritable;

	/**
		If `true`, specifies that the `output` should be treated as a TTY terminal.

		Default: checking the value of the `isTTY` property on the `output` stream upon instantiation.
	**/
	@:optional var terminal:Bool;

	/**
		The function to be used when evaluating each given line of input.

		Default: an async wrapper for the JavaScript `eval()` function.
	**/
	#if haxe4
	@:optional var eval:(code:String, context:DynamicAccess<Dynamic>, file:String, cb:(error:Null<Error>, ?result:Dynamic) -> Void) -> Void;
	#else
	@:optional var eval:String->DynamicAccess<Dynamic>->String->(Null<Error>->?Dynamic->Void)->Void;
	#end

	/**
		If `true`, specifies that the default `writer` function should include ANSI color styling to REPL output.
		If a custom `writer` function is provided then this has no effect.

		Default: checking color support on the `output` stream if the REPL instance's `terminal` value is `true`.
	**/
	@:optional var useColors:Bool;

	/**
		If `true`, specifies that the default evaluation function will use the JavaScript `global` as the context as
		opposed to creating a new separate context for the REPL instance.
		The node CLI REPL sets this value to `true`.

		Default: `false`.
	**/
	@:optional var useGlobal:Bool;

	/**
		If `true`, specifies that the default writer will not output the return value of a command if it evaluates to
		`undefined`.

		Default: `false`.
	**/
	@:optional var ignoreUndefined:Bool;

	/**
		The function to invoke to format the output of each command before writing to `output`.

		Default: `util.inspect()`.
	**/
	@:optional var writer:Dynamic->Void;

	/**
		An optional function used for custom Tab auto completion.
	**/
	@:optional var completer:Readline.ReadlineCompleterCallback;

	/**
		A flag that specifies whether the default evaluator executes all JavaScript commands in strict mode or default
		(sloppy) mode.
		Acceptable values are `repl.REPL_MODE_SLOPPY` or `repl.REPL_MODE_STRICT`.
	**/
	#if haxe4
	@:optional var replMode:Symbol;
	#else
	@:optional var replMode:Dynamic;
	#end

	/**
		Stop evaluating the current piece of code when `SIGINT` is received, i.e. `Ctrl+C` is pressed.
		This cannot be used together with a custom `eval` function.

		Default: `false`.
	**/
	@:optional var breakEvalOnSigint:Bool;
}
