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

package js.node.repl;

import haxe.DynamicAccess;
import js.node.events.EventEmitter;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Enumeration of events emitted by the `REPLServer` objects.
**/
enum abstract REPLServerEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		The `'exit'` event is emitted when the REPL is exited either by receiving the `.exit` command as input,
		the user pressing `<ctrl>-C` twice to signal `SIGINT`, or by pressing `<ctrl>-D` to signal 'end' on the input stream.
		The listener callback is invoked without any arguments.

		@see https://nodejs.org/api/repl.html#repl_event_exit
	**/
	var Exit:REPLServerEvent<Void->Void> = "exit";

	/**
		The `'reset'` event is emitted when the REPL's context is reset.
		This occurs whenever the `.clear` command is received as input unless the REPL is using the default evaluator
		and the `repl.REPLServer` instance was created with the `useGlobal` option set to `true`.
		The listener callback will be called with a reference to the `context` object as the only argument.

		@see https://nodejs.org/api/repl.html#repl_event_reset
	**/
	#if haxe4
	var Reset:REPLServerEvent<(context:DynamicAccess<Dynamic>) -> Void> = "reset";
	#else
	var Reset:REPLServerEvent<DynamicAccess<Dynamic>->Void> = "reset";
	#end
}

/**
	Instances of `repl.REPLServer` are created using the `repl.start()` method and should not be created directly using
	the JavaScript `new` keyword.

	@see https://nodejs.org/api/repl.html#repl_class_replserver
**/
@:jsRequire("repl", "REPLServer")
extern class REPLServer extends EventEmitter<REPLServer> {
	/**
		It is possible to expose a variable to the REPL explicitly by assigning it to the `context` object associated
		with each `REPLServer`.

		@see https://nodejs.org/api/repl.html#repl_global_and_local_scope
	**/
	var context(default, null):DynamicAccess<Dynamic>;

	/**
		The `replServer.defineCommand()` method is used to add new `.`-prefixed commands to the REPL instance.

		@see https://nodejs.org/api/repl.html#repl_replserver_definecommand_keyword_cmd
	**/
	#if haxe4
	@:overload(function(keyword:String, cmd:(rest:String) -> Void):Void {})
	#else
	@:overload(function(keyword:String, cmd:String->Void):Void {})
	#end
	function defineCommand(keyword:String, cmd:REPLServerOptions):Void;

	/**
		The `replServer.displayPrompt()` method readies the REPL instance for input from the user, printing the
		configured `prompt` to a new line in the `output` and resuming the `input` to accept new input.

		@see https://nodejs.org/api/repl.html#repl_replserver_displayprompt_preservecursor
	**/
	function displayPrompt(?preserveCursor:Bool):Void;

	/**
		The `replServer.clearBufferedCommand()` method clears any command that has been buffered but not yet executed.

		@see https://nodejs.org/api/repl.html#repl_replserver_clearbufferedcommand
	**/
	function clearBufferedCommand():Void;

	/**
		Initializes a history log file for the REPL instance.

		@see https://nodejs.org/api/repl.html#repl_replserver_setuphistory_historypath_callback
	**/
	#if haxe4
	function setupHistory(historyPath:String, callback:(err:Null<Error>, repl:Null<REPLServer>) -> Void):Void;
	#else
	function setupHistory(historyPath:String, callback:Null<Error>->Null<REPLServer>->Void):Void;
	#end
}

/**
	Options object used by `REPLServer.defineCommand`.

	@see https://nodejs.org/api/repl.html#repl_class_replserver
**/
typedef REPLServerOptions = {
	/**
		Help text to be displayed when `.help` is entered.
	**/
	@:optional var help:String;

	/**
		The function to execute.
	**/
	#if haxe4
	var action:(rest:String) -> Void;
	#else
	var action:String->Void;
	#end
}
