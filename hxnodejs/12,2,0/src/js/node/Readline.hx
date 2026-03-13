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

import haxe.extern.EitherType;
import js.node.readline.*;
import js.node.stream.Readable.IReadable;
import js.node.stream.Writable.IWritable;

/**
	The readline module provides an interface for reading data from a `Readable` stream (such as `process.stdin`) one
	line at a time.

	@see https://nodejs.org/api/readline.html#readline_readline
**/
@:jsRequire("readline")
extern class Readline {
	/**
		The `readline.clearLine()` method Clears current line of given `TTY` stream
		in a specified direction identified by `dir`.

		@see https://nodejs.org/api/readline.html#readline_readline_clearline_stream_dir_callback
	**/
	static function clearLine(stream:IWritable, dir:ClearLineDirection, ?callback:Void->Void):Bool;

	/**
		The `readline.clearScreenDown()` method clears the given `TTY` stream
		from the current position of the cursor down.

		@see https://nodejs.org/api/readline.html#readline_readline_clearscreendown_stream_callback
	**/
	static function clearScreenDown(stream:IWritable, ?callback:Void->Void):Bool;

	/**
		The `readline.createInterface()` method creates a new `readline.Interface` instance.

		@see https://nodejs.org/api/readline.html#readline_readline_createinterface_options
	**/
	@:overload(function(input:IReadable, ?output:IWritable, ?completer:ReadlineCompleterCallback, ?terminal:Bool):Interface {})
	static function createInterface(options:ReadlineOptions):Interface;

	/**
		The `readline.cursorTo()` method moves cursor to the specified position in a given `TTY` `stream`.

		@see https://nodejs.org/api/readline.html#readline_readline_cursorto_stream_x_y_callback
	**/
	static function cursorTo(stream:IWritable, x:Int, ?y:Int, ?callback:Void->Void):Bool;

	/**
		The `readline.emitKeypressEvents()` method causes the given `Readable` stream to begin emitting `'keypress'`
		events corresponding to received input.

		@see https://nodejs.org/api/readline.html#readline_readline_emitkeypressevents_stream_interface
	**/
	static function emitKeypressEvents(stream:IReadable, ?iface:Interface):Void;

	/**
		The `readline.moveCursor()` method moves the cursor relative to its current position in a given `TTY` `stream`.

		@see https://nodejs.org/api/readline.html#readline_readline_movecursor_stream_dx_dy_callback
	**/
	static function moveCursor(stream:IWritable, dx:Int, dy:Int, ?callback:Void->Void):Bool;
}

/**
	Options object used by `Readline.createInterface`.

	@see https://nodejs.org/api/readline.html#readline_readline_createinterface_options
**/
typedef ReadlineOptions = {
	/**
		The `Readable` stream to listen to.
	**/
	var input:IReadable;

	/**
		The `Writable` stream to write readline data to.
	**/
	@:optional var output:IWritable;

	/**
		An optional function used for Tab autocompletion.

		@see https://nodejs.org/api/readline.html#readline_use_of_the_completer_function
	**/
	@:optional var completer:ReadlineCompleterCallback;

	/**
		`true` if the `input` and `output` streams should be treated like a TTY, and have ANSI/VT100 escape codes
		written to it.

		Default: checking `isTTY` on the `output` stream upon instantiation.
	**/
	@:optional var terminal:Bool;

	/**
		Maximum number of history lines retained.
		To disable the history set this value to `0`.
		This option makes sense only if `terminal` is set to `true` by the user or by an internal `output` check,
		otherwise the history caching mechanism is not initialized at all.

		Default: `30`.
	**/
	@:optional var historySize:Int;

	/**
		The prompt string to use.

		Default: `'> '`.
	**/
	@:optional var prompt:String;

	/**
		If the delay between `\r` and `\n` exceeds `crlfDelay` milliseconds, both `\r` and `\n` will be treated as
		separate end-of-line input.
		`crlfDelay` will be coerced to a number no less than `100`.
		It can be set to `Infinity`, in which case `\r` followed by `\n` will always be considered a single newline
		(which may be reasonable for reading files with `\r\n` line delimiter).

		Default: `100`.
	**/
	@:optional var crlfDelay:Int;

	/**
		If `true`, when a new input line added to the history list duplicates an older one, this removes the older line
		from the list.

		Default: `false`.
	**/
	@:optional var removeHistoryDuplicates:Bool;

	/**
		The duration `readline` will wait for a character (when reading an ambiguous key sequence in milliseconds one
		that can both form a complete key sequence using the input read so far and can take additional input to complete
		a longer key sequence).

		Default: `500`.
	**/
	@:optional var escapeCodeTimeout:Int;
}

#if haxe4
typedef ReadlineCompleterCallback = (line:String) -> Array<EitherType<Array<String>, String>>;
#else
typedef ReadlineCompleterCallback = String->Array<EitherType<Array<String>, String>>;
#end

/**
	Enumeration of possible directions for `Readline.clearLine`.

	@see https://nodejs.org/api/readline.html#readline_readline_clearline_stream_dir_callback
**/
enum abstract ClearLineDirection(Int) from Int to Int {
	/**
		to the left from cursor.
	**/
	var Left = -1;

	/**
		to the right from cursor.
	**/
	var Right = 1;

	/**
		the entire line.
	**/
	var EntireLine = 0;
}
