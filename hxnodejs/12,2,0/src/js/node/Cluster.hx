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
import js.node.cluster.Worker;
import js.node.events.EventEmitter;

enum abstract ClusterEvent<T:haxe.Constraints.Function>(Event<T>) to Event<T> {
	/**
		When a new worker is forked the cluster module will emit a 'fork' event.
		This can be used to log worker activity, and create your own timeout.

		Listener arguments:
			* worker:Worker
	**/
	var Fork:ClusterEvent<Worker->Void> = "fork";

	/**
		After forking a new worker, the worker should respond with an online message.
		When the master receives an online message it will emit this event.

		The difference between 'fork' and 'online' is that fork is emitted when the master forks a worker,
		and 'online' is emitted when the worker is running.

		Listener arguments:
			* worker:Worker
	**/
	var Online:ClusterEvent<Worker->Void> = "online";

	/**
		After calling `listen` from a worker, when the 'listening' event is emitted on the server,
		a listening event will also be emitted on cluster in the master.

		The event handler is executed with two arguments, the `worker` contains the worker object and
		the `address` object contains the following connection properties: address, port and addressType.
		This is very useful if the worker is listening on more than one address.

		 		Listener arguments:
			* worker:Worker
			* address:ListeningEventAddress
	**/
	var Listening:ClusterEvent<Worker->ListeningEventAddress->Void> = "listening";

	/**
		Emitted after the worker IPC channel has disconnected.

		This can occur when a worker exits gracefully, is killed,
		or is disconnected manually (such as with `Worker.disconnect`).

		There may be a delay between the 'disconnect' and 'exit' events.

		These events can be used to detect if the process is stuck in a cleanup
		or if there are long-living connections.

		Listener arguments:
			* worker:Worker
	**/
	var Disconnect:ClusterEvent<Worker->Void> = "disconnect";

	/**
		When any of the workers die the cluster module will emit the 'exit' event.
		This can be used to restart the worker by calling `Cluster.fork` again.

		Listener arguments:
			* worker:Worker
			* code:Int - the exit code, if it exited normally.
			* signal:String - the name of the signal (eg. 'SIGHUP') that caused the process to be killed.
	**/
	var Exit:ClusterEvent<Worker->Int->String->Void> = "exit";

	/**
		Emitted the first time that `Cluster.setupMaster` is called.
	**/
	var Setup:ClusterEvent<ClusterSettings->Void> = "setup";
}

/**
	Structure emitted by 'listening' event.
**/
typedef ListeningEventAddress = {
	var address:String;
	var port:Int;
	var addressType:ListeningEventAddressType;
}

enum abstract ListeningEventAddressType(haxe.extern.EitherType<Int, String>) to haxe.extern.EitherType<Int, String> {
	var TCPv4 = 4;
	var TCPv6 = 6;
	var Unix = -1;
	var UDPv4 = "udp4";
	var UDPv6 = "udp6";
}

@:jsRequire("cluster")
extern enum abstract ClusterSchedulingPolicy(Int) {
	var SCHED_NONE;
	var SCHED_RR;
}

/**
	A single instance of Node runs in a single thread.
	To take advantage of multi-core systems the user will sometimes want to launch a cluster of Node processes to handle the load.
	The cluster module allows you to easily create child processes that all share server ports.

	This feature was introduced recently, and may change in future versions. Please try it out and provide feedback.

	Also note that, on Windows, it is not yet possible to set up a named pipe server in a worker.
**/
@:jsRequire("cluster")
extern class Cluster extends EventEmitter<Cluster> {
	/**
		A reference to the `Cluster` object returned by node.js module.

		It can be imported into module namespace by using: "import js.node.Cluster.instance in cluster"
	**/
	public static inline var instance:Cluster = cast Cluster;

	/**
		The scheduling policy, either `SCHED_RR` for round-robin
		or `SCHED_NONE` to leave it to the operating system.

		This is a global setting and effectively frozen once you spawn the first worker
		or call `setupMaster`, whatever comes first.

		`SCHED_RR` is the default on all operating systems except Windows.
		Windows will change to `SCHED_RR` once libuv is able to effectively distribute IOCP handles
		without incurring a large performance hit.

		`schedulingPolicy` can also be set through the NODE_CLUSTER_SCHED_POLICY environment variable.
		Valid values are "rr" and "none".
	**/
	var schedulingPolicy:ClusterSchedulingPolicy;

	/**
		After calling `setupMaster` (or `fork`) this settings object will contain the settings, including the default values.

		It is effectively frozen after being set, because `setupMaster` can only be called once.

		This object is not supposed to be changed or set manually, by you.
	**/
	var settings(default, null):ClusterSettings;

	/**
		True if the process is a master.
		This is determined by the process.env.NODE_UNIQUE_ID.
		If process.env.NODE_UNIQUE_ID is undefined, then `isMaster` is true.
	**/
	var isMaster(default, null):Bool;

	/**
		True if the process is not a master (it is the negation of `isMaster`).
	**/
	var isWorker(default, null):Bool;

	/**
		`setupMaster` is used to change the default `fork` behavior.

		Once called, the `settings` will be present in `settings`.

		Note that:
			Only the first call to `setupMaster` has any effect, subsequent calls are ignored

			That because of the above, the only attribute of a worker that may be customized per-worker
			is the `env` passed to `fork`

			`fork` calls `setupMaster` internally to establish the defaults, so to have any effect,
			`setupMaster` must be called before any calls to `fork`
	**/
	function setupMaster(?settings:{?exec:String, ?args:Array<String>, ?silent:Bool}):Void;

	/**
		Spawn a new worker process.

		This can only be called from the master process.
	**/
	function fork(?env:DynamicAccess<String>):Worker;

	/**
		Calls `disconnect` on each worker in `workers`.

		When they are disconnected all internal handles will be closed,
		allowing the master process to die gracefully if no other event is waiting.

		The method takes an optional `callback` argument which will be called when finished.

		This can only be called from the master process.
	**/
	function disconnect(?callback:Void->Void):Void;

	/**
		A reference to the current worker object.

		Not available in the master process.
	**/
	var worker(default, null):Worker;

	/**
		A hash that stores the active worker objects, keyed by `id` field.
		Makes it easy to loop through all the workers.

		It is only available in the master process.

		A worker is removed from `workers` just before the 'disconnect' or 'exit' event is emitted.

		Should you wish to reference a worker over a communication channel, using the worker's unique `id`
		is the easiest way to find the worker.
	**/
	var workers(default, null):DynamicAccess<Worker>;
}

typedef ClusterSettings = {
	/**
		list of string arguments passed to the node executable.
		Default: process.execArgv
	**/
	@:optional var execArgv(default, null):Array<String>;

	/**
		file path to worker file.
		Default: process.argv[1]
	**/
	@:optional var exec(default, null):String;

	/**
		string arguments passed to worker.
		Default: process.argv.slice(2)
	**/
	@:optional var args(default, null):Array<String>;

	/**
		whether or not to send output to parent's stdio.
		Default: false
	**/
	@:optional var silent(default, null):Bool;

	/**
		Sets the user identity of the process.
	**/
	@:optional var uid(default, null):Int;

	/**
		Sets the group identity of the process.
	**/
	@:optional var gid(default, null):Int;
}
