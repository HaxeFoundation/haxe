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

package js.node.child_process;

import js.node.Stream;
import js.node.events.EventEmitter;
import js.node.stream.Readable;
import js.node.stream.Writable;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Enumeration of events emitted by `ChildProcess` objects.
**/
enum abstract ChildProcessEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		Emitted when:
			1. The process could not be spawned, or
			2. The process could not be killed, or
			3. Sending a message to the child process failed for whatever reason.

		Note that the exit-event may or may not fire after an error has occured.
		If you are listening on both events to fire a function, remember to guard against calling your function twice.

		See also `ChildProcess.kill` and `ChildProcess.send`.
	**/
	var Error:ChildProcessEvent<Error->Void> = "error";

	/**
		This event is emitted after the child process ends.

		Listener arguments:
			code - the exit code, if it exited normally.
			signal - the signal passed to kill the child process, if it was killed by the parent.

		If the process terminated normally, `code` is the final exit code of the process, otherwise null.
		If the process terminated due to receipt of a signal, `signal` is the string name of the signal, otherwise null.

		Note that the child process stdio streams might still be open.

		Also, note that node establishes signal handlers for 'SIGINT' and 'SIGTERM',
		so it will not terminate due to receipt of those signals, it will exit.
		See waitpid(2).
	**/
	var Exit:ChildProcessEvent<Int->String->Void> = "exit";

	/**
		This event is emitted when the stdio streams of a child process have all terminated.
		This is distinct from `Exit`, since multiple processes might share the same stdio streams.

		Listener arguments:
			code - the exit code, if it exited normally.
			signal - the signal passed to kill the child process, if it was killed by the parent.
	**/
	var Close:ChildProcessEvent<Int->String->Void> = "close";

	/**
		This event is emitted after calling the `disconnect` method in the parent or in the child.
		After disconnecting it is no longer possible to send messages, and the `connected` property is false.
	**/
	var Disconnect:ChildProcessEvent<Void->Void> = "disconnect";

	/**
		Messages send by `send` are obtained using the message event.

		This event can also be listened on the `process` object to receive messages from the parent.

		Listener arguments:
			message - a parsed JSON object or primitive value
			sendHandle - a Socket or Server object
	**/
	var Message:ChildProcessEvent<Dynamic->Dynamic->Void> = "message";
}

typedef ChildProcessSendOptions = {
	/**
		Can be used when passing instances of `js.node.net.Socket`.

		When true, the socket is kept open in the sending process.

		Defaults to false.
	**/
	@:optional var keepOpen:Bool;
}

/**
	An object representing a child process.

	The `ChildProcess` class is not intended to be used directly. Use the spawn() or fork() module methods
	to create a `ChildProcess` instance.
**/
extern class ChildProcess extends EventEmitter<ChildProcess> {
	/**
		A Writable Stream that represents the child process's stdin.
		Closing this stream via `end` often causes the child process to terminate.

		If the child stdio streams are shared with the parent, then this will not be set.
	**/
	var stdin(default, null):IWritable;

	/**
		A Readable Stream that represents the child process's stdout.

		If the child stdio streams are shared with the parent, then this will not be set.
	**/
	var stdout(default, null):IReadable;

	/**
		A Readable Stream that represents the child process's stderr.

		If the child stdio streams are shared with the parent, then this will not be set.
	**/
	var stderr(default, null):IReadable;

	/**
		The parent end of the stdio pipes.
	**/
	var stdio(default, null):Array<IStream>;

	/**
		The PID of the child process.
	**/
	var pid(default, null):Int;

	/**
		Set to false after `disconnect' is called
		If `connected` is false, it is no longer possible to send messages.
	**/
	var connected(default, null):Bool;

	/**
		Send a signal to the child process.

		If no argument is given, the process will be sent 'SIGTERM'.
		See signal(7) for a list of available signals.

		May emit an 'error' event when the signal cannot be delivered.

		Sending a signal to a child process that has already exited is not an error
		but may have unforeseen consequences: if the PID (the process ID) has been reassigned to another process,
		the signal will be delivered to that process instead. What happens next is anyone's guess.

		Note that while the function is called `kill`, the signal delivered to the child process may not actually kill it.
		`kill` really just sends a signal to a process. See kill(2)
	**/
	function kill(?signal:String):Void;

	/**
		When using `fork` you can write to the child using `send` and messages are received by a 'message' event on the child.

		In the child the `Process` object will have a `send` method, and process will emit objects each time it receives
		a message on its channel.

		Please note that the `send` method on both the parent and child are synchronous - sending large chunks of data is
		not advised (pipes can be used instead, see `spawn`).

		There is a special case when sending a {cmd: 'NODE_foo'} `message`. All messages containing a `NODE_` prefix in
		its cmd property will not be emitted in the 'message' event, since they are internal messages used by node core.
		Messages containing the prefix are emitted in the 'internalMessage' event, you should by all means avoid using
		this feature, it is subject to change without notice.

		The `sendHandle` option is for sending a TCP server or socket object to another process.
		The child will receive the object as its second argument to the message event.

		The `callback` option is a function that is invoked after the message is sent but before the target may have received it.
		It is called with a single argument: null on success, or an `Error` object on failure.

		Emits an 'error' event if the message cannot be sent, for example because the child process has already exited.

		Returns true under normal circumstances or false when the backlog of unsent messages exceeds a threshold that
		makes it unwise to send more. Use the callback mechanism to implement flow control.
	**/
	@:overload(function(message:Dynamic, sendHandle:Dynamic, options:ChildProcessSendOptions, ?callback:Error->Void):Bool {})
	@:overload(function(message:Dynamic, sendHandle:Dynamic, ?callback:Error->Void):Bool {})
	function send(message:Dynamic, ?callback:Error->Void):Bool;

	/**
		Close the IPC channel between parent and child, allowing the child to exit gracefully once there are no other
		connections keeping it alive.

		After calling this method the `connected` flag will be set to false in both the parent and child,
		and it is no longer possible to send messages.

		The 'disconnect' event will be emitted when there are no messages in the process of being received,
		most likely immediately.

		Note that you can also call `process.disconnect` in the child process.
	**/
	function disconnect():Void;

	/**
		By default, the parent will wait for the detached child to exit.
		To prevent the parent from waiting for a given child, use the `unref` method,
		and the parent's event loop will not include the child in its reference count.
	**/
	function unref():Void;
}
