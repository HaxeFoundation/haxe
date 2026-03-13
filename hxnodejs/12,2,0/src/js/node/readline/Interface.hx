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

package js.node.readline;

import js.node.Buffer;
import js.node.events.EventEmitter;

/**
	Enumeration of events emitted by the `Interface` objects.
**/
enum abstract InterfaceEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		The `'close'` event is emitted when one of the following occur:

		@see https://nodejs.org/api/readline.html#readline_event_close
	**/
	var Close:InterfaceEvent<Void->Void> = "close";

	/**
		The `'line'` event is emitted whenever the `input` stream receives an end-of-line input (`\n`, `\r`, or `\r\n`).

		@see https://nodejs.org/api/readline.html#readline_event_line
	**/
	var Line:InterfaceEvent<String->Void> = "line";

	/**
		The `'pause'` event is emitted when one of the following occur:

		@see https://nodejs.org/api/readline.html#readline_event_pause
	**/
	var Pause:InterfaceEvent<Void->Void> = "pause";

	/**
		The `'resume'` event is emitted whenever the `input` stream is resumed.

		@see https://nodejs.org/api/readline.html#readline_event_resume
	**/
	var Resume:InterfaceEvent<Void->Void> = "resume";

	/**
		The `'SIGCONT'` event is emitted when a Node.js process previously moved into the background using `<ctrl>-Z`
		(i.e. `SIGTSTP`) is then brought back to the foreground using `fg(1p)`.

		@see https://nodejs.org/api/readline.html#readline_event_sigcont
	**/
	var SIGCONT:InterfaceEvent<Void->Void> = "SIGCONT";

	/**
		The `'SIGINT'` event is emitted whenever the `input` stream receives a `<ctrl>-C` input, known typically as
		`SIGINT`.

		@see https://nodejs.org/api/readline.html#readline_event_sigint
	**/
	var SIGINT:InterfaceEvent<Void->Void> = "SIGINT";

	/**
		The `'SIGTSTP'` event is emitted when the `input` stream receives a `<ctrl>-Z` input, typically known as
		`SIGTSTP`.

		@see https://nodejs.org/api/readline.html#readline_event_sigtstp
	**/
	var SIGTSTP:InterfaceEvent<Void->Void> = "SIGTSTP";
}

/**
	Instances of the `readline.Interface` class are constructed using the `readline.createInterface()` method.

	@see https://nodejs.org/api/readline.html#readline_class_interface
**/
extern class Interface extends EventEmitter<Interface> {
	/**
		The `rl.close()` method closes the `readline.Interface` instance and relinquishes control over the `input` and
		`output` streams.

		@see https://nodejs.org/api/readline.html#readline_rl_close
	**/
	function close():Void;

	/**
		The `rl.pause()` method pauses the `input` stream, allowing it to be resumed later if necessary.

		@see https://nodejs.org/api/readline.html#readline_rl_pause
	**/
	function pause():Void;

	/**
		The `rl.prompt()` method writes the `readline.Interface` instances configured `prompt` to a new line in `output`
		in order to provide a user with a new location at which to provide input.

		@see https://nodejs.org/api/readline.html#readline_rl_prompt_preservecursor
	**/
	function prompt(?preserveCursor:Bool):Void;

	/**
		The `rl.question()` method displays the `query` by writing it to the `output`, waits for user `input` to be
		provided on input, then invokes the `callback` function passing the provided input as the first argument.

		@see https://nodejs.org/api/readline.html#readline_rl_question_query_callback
	**/
	function question(query:String, callback:String->Void):Void;

	/**
		The `rl.resume()` method resumes the `input` stream if it has been paused.

		@see https://nodejs.org/api/readline.html#readline_rl_resume
	**/
	function resume():Void;

	/**
		The `rl.setPrompt()` method sets the prompt that will be written to `output` whenever `rl.prompt()` is called.

		@see https://nodejs.org/api/readline.html#readline_rl_setprompt_prompt
	**/
	function setPrompt(prompt:String):Void;

	/**
		The `rl.write()` method write either `data` or a key sequence identified by `key` to the `output`.

		@see https://nodejs.org/api/readline.html#readline_rl_write_data_key
	**/
	@:overload(function(data:Buffer, ?key:InterfaceWriteKey):Void {})
	function write(data:String, ?key:InterfaceWriteKey):Void;
}

/**
	Key sequence passed as the `key` argument to `Interface.write`.

	@see https://nodejs.org/api/readline.html#readline_rl_write_data_key
**/
typedef InterfaceWriteKey = {
	/**
		`true` to indicate the <ctrl> key.
	**/
	@:optional var ctrl:Bool;

	/**
		`true` to indicate the <Meta> key.
	 */
	@:optional var meta:Bool;

	/**
		`true` to indicate the <Shift> key.
	**/
	@:optional var shift:Bool;

	/**
		The name of the a key.
	**/
	var name:String;
}
