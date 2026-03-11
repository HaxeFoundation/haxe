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

import haxe.Exception;
#if (!target.threaded)
#error "This class is not available on this target"
#end

function withMutex<T>(f:() -> T) {
	@:privateAccess Thread.mutex.acquire(); // TODO: use other mutex?
	final v = f();
	@:privateAccess Thread.mutex.release();
	return v;
}

class ThreadCallback<F> implements IThreadCallbackHandle {
	public final callback:F;
	public var next:Null<ThreadCallback<F>>;
	public var prev:Null<ThreadCallback<F>>;
	public var isClosed(get, null):Bool;

	final host:ThreadCallbackStack<F>;

	public function new(host:ThreadCallbackStack<F>, callback:F, ?prev:ThreadCallback<F>) {
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

class ThreadCallbackStack<F> {
	var top:Null<ThreadCallback<F>>;

	public function new() {}

	public function add(f:F) {
		return withMutex(() -> {
			top = new ThreadCallback(this, f, top);
		});
	}

	public function foreach(f:ThreadCallback<F>->Void) {
		var current = top;
		while (current != null) {
			// Save prev before calling f, in case f or a concurrent close modifies the list.
			// Skip closed entries so that closing a handle stops future invocations.
			final prev = current.prev;
			if (!current.isClosed) f(current);
			current = prev;
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

class ThreadCallbackManager {
	public var onCreateCallback:Null<ThreadCallbackStack<() -> Void>>;
	public var onStartCallback:Null<ThreadCallbackStack<() -> Void>>;
	public var onJobDoneCallback:Null<ThreadCallbackStack<() -> Void>>;
	public var onExitCallback:Null<ThreadCallbackStack<() -> Void>>;
	public var onAbortCallback:Null<ThreadCallbackStack<haxe.Exception -> Void>>;

	public function new() {}

	public function onCreate(f:() -> Void):IThreadCallbackHandle {
		onCreateCallback ??= new ThreadCallbackStack();
		return onCreateCallback.add(f);
	}

	public function onStart(f:() -> Void):IThreadCallbackHandle {
		onStartCallback ??= new ThreadCallbackStack();
		return onStartCallback.add(f);
	}

	public function onJobDone(f:() -> Void):IThreadCallbackHandle {
		onJobDoneCallback ??= new ThreadCallbackStack();
		return onJobDoneCallback.add(f);
	}

	public function onExit(f:() -> Void):IThreadCallbackHandle {
		onExitCallback ??= new ThreadCallbackStack();
		return onExitCallback.add(f);
	}

	public function onAbort(f:haxe.Exception -> Void):IThreadCallbackHandle {
		onAbortCallback ??= new ThreadCallbackStack();
		return onAbortCallback.add(f);
	}

	static function collectCallbacks<F>(local:Null<ThreadCallbackStack<F>>, global:Null<ThreadCallbackStack<F>>) {
		final toExecute = [];
		withMutex(() -> {
			if (local != null) {
				local.foreach(c -> toExecute.push(c));
			}
			if (global != null) {
				global.foreach(c -> toExecute.push(c));
			}
		});
		return toExecute;
	}

	static function iterateCallbacks<F>(callbacks:Array<ThreadCallback<F>>, f:ThreadCallback<F> -> Void) {
		var firstException = null;
		for (c in callbacks) {
			if (!c.isClosed) {
				try {
					f(c);
				} catch(e:Exception) {
					if (firstException == null) {
						firstException = e;
					}
				}
			}
		}
		if (firstException != null) {
			throw firstException;
		}
	}

	static public function invokeCallbacks(local:Null<ThreadCallbackStack<() -> Void>>, global:Null<ThreadCallbackStack<() -> Void>>) {
		final callbacks = collectCallbacks(local, global);
		iterateCallbacks(callbacks, c -> c.callback());
	}

	static public function invokeCallbacksArg<Arg>(local:Null<ThreadCallbackStack<Arg -> Void>>, global:Null<ThreadCallbackStack<Arg -> Void>>, e:Arg) {
		final callbacks = collectCallbacks(local, global);
		iterateCallbacks(callbacks, c -> c.callback(e));
	}
}

class MultiHandle implements IThreadCallbackHandle {
	final handles:Array<IThreadCallbackHandle>;

	public function new(handles:Array<IThreadCallbackHandle>) {
		this.handles = handles;
	}

	public var isClosed(get, never):Bool;

	function get_isClosed() {
		for (h in handles)
			if (!h.isClosed) return false;
		return true;
	}

	public function close() {
		for (h in handles)
			h.close();
	}
}
