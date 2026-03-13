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

package js.node.cluster;

import js.node.Cluster.ListeningEventAddress;
import js.node.child_process.ChildProcess;
import js.node.events.EventEmitter;
#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

enum abstract WorkerEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	var Message:WorkerEvent<Dynamic->Dynamic->Void> = "message";
	var Online:WorkerEvent<Void->Void> = "online";
	var Listening:WorkerEvent<ListeningEventAddress->Void> = "listening";
	var Disconnect:WorkerEvent<Void->Void> = "disconnect";
	var Exit:WorkerEvent<Int->String->Void> = "exit";
	var Error:WorkerEvent<Error->Void> = "error";
}

/**
	A Worker object contains all public information and method about a worker.
	In the master it can be obtained using `Cluster.workers`.
	In a worker it can be obtained using `Cluster.worker`.
**/
extern class Worker extends EventEmitter<Worker> {
	/**
		Each new worker is given its own unique id, this id is stored in the `id`.

		While a worker is alive, this is the key that indexes it in `Cluster.workers`
	**/
	var id(default, null):String;

	/**
		All workers are created using `ChildProcess.fork`, the returned object from this function is stored as `process`.
		In a worker, the global process is stored.

		Note that workers will call `process.exit(0)` if the 'disconnect' event occurs on process
		and `suicide` is not true. This protects against accidental disconnection.
	**/
	var process:ChildProcess;

	/**
		Set by calling `kill` or `disconnect`, until then it is undefined.

		Lets you distinguish between voluntary and accidental exit, the master may choose
		not to respawn a worker based on this value.

		(an alias to `exitedAfterDisconnect` in newer node.js versions)
	**/
	var suicide:Null<Bool>;

	/**
		Set by calling `kill` or `disconnect`. Until then, it is undefined.

		Lets you distinguish between voluntary and accidental exit, the master may choose
		not to respawn a worker based on this value.
	**/
	var exitedAfterDisconnect:Null<Bool>;

	/**
		This function is equal to the `send` methods provided by `ChildProcess.fork`.
		In the master you should use this function to send a `message` to a specific worker.

		In a worker you can also use `process.send`, it is the same function.
	**/
	@:overload(function(message:Dynamic, sendHandle:Dynamic, ?callback:Error->Void):Bool {})
	function send(message:Dynamic, ?callback:Error->Void):Bool;

	/**
		This function will kill the worker. In the master, it does this by disconnecting the `worker.process`,
		and once disconnected, killing with `signal`. In the worker, it does it by disconnecting the channel,
		and then exiting with code 0.

		Causes `suicide` to be set.
	**/
	function kill(?signal:String):Void;

	/**
		In a worker, this function will close all servers, wait for the 'close' event on those servers,
		and then disconnect the IPC channel.

		In the master, an internal message is sent to the worker causing it to call `disconnect` on itself.

		Causes `suicide` to be set.
	**/
	function disconnect():Void;

	/**
		This function returns true if the worker is connected to its master via its IPC channel, false otherwise

		A worker is connected to its master after it's been created.
		It is disconnected after the 'disconnect' event is emitted.
	**/
	function isConnected():Bool;

	/**
		This function returns true if the worker's process has terminated (either because of exiting or being signaled).
		Otherwise, it returns false.
	**/
	function isDead():Bool;
}
