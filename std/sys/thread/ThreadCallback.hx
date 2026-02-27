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

function withMutex<T>(f:() -> T) {
	@:privateAccess Thread.mutex.acquire(); // TODO: use other mutex?
	final v = f();
	@:privateAccess Thread.mutex.release();
	return v;
}

private class ThreadCallback<F> implements IThreadCallbackHandle {
	public final callback:F;
	public var next:Null<ThreadCallback<F>>;
	public var prev:Null<ThreadCallback<F>>;
	public var isClosed(get, null):Bool;

	final host:ThreadCallbacks<F>;

	public function new(host:ThreadCallbacks<F>, callback:F, ?prev:ThreadCallback<F>) {
		this.host = host;
		this.callback = callback;
		this.prev = prev;
		if (prev != null) {
			prev.next = this;
		}
	}

	function get_isClosed() {
		return isClosed;
	}

	public function close() {
		withMutex(() -> {
			if (isClosed) {
				return;
			}
			isClosed = true;
			if (prev != null) {
				prev.next = next;
			}
			if (next != null) {
				next.prev = prev;
			} else {
				@:privateAccess host.top = prev;
			}
		});
	}
}

class ThreadCallbacks<F> {
	var top:Null<ThreadCallback<F>>;

	public function new() {}

	public function add(f:F) {
		return withMutex(() -> {
			top = new ThreadCallback(this, f, top);
		});
	}

	public function foreach(f:F->Void) {
		var current = top;
		while (current != null) {
			f(current.callback);
			current = current.prev;
		};
	}

}

interface IThreadCallbackHandle {
	/**
		Whether this callback has been removed.
	**/
	var isClosed(get, never):Bool;

	/**
		Removes this callback from its parent list.
	**/
	function close():Void;
}

class ThreadInstanceCallbacks {
	var onJobDoneCallback:Null<ThreadCallbacks<() -> Void>>;
	var onExitCallback:Null<ThreadCallbacks<() -> Void>>;

	public function new() {}

	function callOnJobDone() {
		if (onJobDoneCallback != null) {
			onJobDoneCallback.foreach(f -> f());
		}
	}

	function callOnExit() {
		if (onExitCallback != null) {
			onExitCallback.foreach(f -> f());
		}
	}

	/**
		Registers `f` to be called once the thread has completed executing its job
		successfully. It is not called if the thread has thrown an exception.
	**/
	public function onJobDone(f:() -> Void):IThreadCallbackHandle {
		onJobDoneCallback ??= new ThreadCallbacks();
		return onJobDoneCallback.add(f);
	}

	/**
		Registers `f` to be called when the thread is exiting. In the case of an exception,
		it is called after `onAbort`.

		It is not guaranteed to be called if the thread is killed in a way that does not lead to
		normal termination. Any callback assigned to this should not throw an exception.
	**/
	public function onExit(f:() -> Void):IThreadCallbackHandle {
		onExitCallback ??= new ThreadCallbacks();
		return onExitCallback.add(f);
	}
}
