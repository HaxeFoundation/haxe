/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package sys.thread;

#if (!target.threaded)
#error "This class is not available on this target"
#end

import haxe.Exception;

/**
	Thread pool with a varying amount of threads.

	A new thread is spawned every time a task is submitted while all existing
	threads are busy.
**/
@:coreApi
class ElasticThreadPool implements IThreadPool {
	/* Amount of alive threads in this pool. */
	public var threadsCount(get,null):Int;
	/* Maximum amount of threads in this pool. */
	public var maxThreadsCount:Int;
	/** Indicates if `shutdown` method of this pool has been called. */
	public var isShutdown(get,never):Bool;
	var _isShutdown = false;
	function get_isShutdown():Bool return _isShutdown;

	final pool:Array<Worker> = [];
	final queue = new Deque<()->Void>();
	final mutex = new Mutex();
	final threadTimeout:Float;

	/**
		Create a new thread pool with `threadsCount` threads.

		If a worker thread does not receive a task for `threadTimeout` seconds it
		is terminated.
	**/
	public function new(maxThreadsCount:Int, threadTimeout:Float = 60):Void {
		if(maxThreadsCount < 1)
			throw new ThreadPoolException('ElasticThreadPool needs maxThreadsCount to be at least 1.');
		this.maxThreadsCount = maxThreadsCount;
		this.threadTimeout = threadTimeout;
	}

	/**
		Submit a task to run in a thread.

		Throws an exception if the pool is shut down.
	**/
	public function run(task:()->Void):Void {
		if(_isShutdown)
			throw new ThreadPoolException('Task is rejected. Thread pool is shut down.');
		if(task == null)
			throw new ThreadPoolException('Task to run must not be null.');

		mutex.acquire();
		var submitted = false;
		var deadWorker = null;
		for(worker in pool) {
			if(deadWorker == null && worker.dead) {
				deadWorker = worker;
			}
			if(worker.task == null) {
				submitted = true;
				worker.wakeup(task);
				break;
			}
		}
		if(!submitted) {
			if(deadWorker != null) {
				deadWorker.wakeup(task);
			} else if(pool.length < maxThreadsCount) {
				var worker = new Worker(queue, threadTimeout);
				pool.push(worker);
				worker.wakeup(task);
			} else {
				queue.add(task);
			}
		}
		mutex.release();
	}

	/**
		Initiates a shutdown.
		All previousely submitted tasks will be executed, but no new tasks will
		be accepted.

		Multiple calls to this method have no effect.
	**/
	public function shutdown():Void {
		if(_isShutdown) return;
		mutex.acquire();
		_isShutdown = true;
		for(worker in pool) {
			worker.shutdown();
		}
		mutex.release();
	}

	function get_threadsCount():Int {
		var result = 0;
		for(worker in pool)
			if(!worker.dead)
				++result;
		return result;
	}
}

private class Worker {
	public var task(default,null):Null<()->Void>;
	public var dead(default,null) = false;

	final deathMutex = new Mutex();
	final waiter = new Lock();
	final queue:Deque<()->Void>;
	final timeout:Float;
	var thread:Thread;
	var isShutdown = false;

	public function new(queue:Deque<()->Void>, timeout:Float) {
		this.queue = queue;
		this.timeout = timeout;
		start();
	}

	public function wakeup(task:()->Void) {
		deathMutex.acquire();
		if(dead)
			start();
		this.task = task;
		waiter.release();
		deathMutex.release();
	}

	public function shutdown() {
		isShutdown = true;
		waiter.release();
	}

	function start() {
		dead = false;
		thread = Thread.create(loop);
	}

	function loop() {
		try {
			while(waiter.wait(timeout)) {
				switch task {
					case null:
						if(isShutdown)
							break;
					case fn:
						fn();
						//if more tasks were added while all threads were busy
						while(true) {
							switch queue.pop(false) {
								case null: break;
								case fn: fn();
							}
						}
						task = null;
				}
			}
			deathMutex.acquire();
			//in case a task was submitted right after the lock timed out
			if(task != null)
				start()
			else
				dead = true;
			deathMutex.release();
		} catch(e) {
			task = null;
			start();
			throw e;
		}
	}
}