package haxe.coro.context;

import haxe.Exception;
import haxe.coro.BaseContinuation;

/**
	An abstract context element that handles exception stack trace management for coroutines.
	`BaseContinuation.startException` and `BaseContinuation.buildCallStack` delegate to this element.

	The default implementation is `DefaultExceptionHandler`.
**/
abstract class ExceptionHandler implements IElement<ExceptionHandler> {
	public static final key = new Key<ExceptionHandler>('ExceptionHandler');

	/**
		Called when an exception is first encountered in a coroutine to process its stack trace.
		Returns the (potentially modified) exception.
	**/
	abstract public function startException(cont:BaseContinuation<Any>, exception:Exception):Exception;

	/**
		Called as an exception propagates up the coroutine continuation chain, to insert each
		continuation frame's stack item into the exception stack.
	**/
	abstract public function buildCallStack(cont:BaseContinuation<Any>):Void;

	public function getKey() {
		return key;
	}
}

/**
	The default `ExceptionHandler` implementation, which reconstructs the coroutine call stack
	from the continuation chain and inserts it into the exception's stack trace.

	`insertIndex` is stored in thread-local storage, making this implementation safe for
	concurrent use across multiple coroutines running on different threads.
**/
class DefaultExceptionHandler extends ExceptionHandler {
	#if debug
	final insertIndex = new haxe.coro.Tls<Int>();
	#end

	public function new() {}

	public function startException(cont:BaseContinuation<Any>, exception:Exception):Exception {
		#if js
		return exception;
		#end
		#if debug
		@:privateAccess cont._hx_startedException = true;

		var stack = [];
		var skipping = 0;
		var localInsertIndex = 0;
		var stackItem = cont.getStackItem();

		/*
			Find first coro stack element
		*/
		var currentFrame:Null<haxe.coro.IStackFrame> = cont;
		while (stackItem == null) {
			currentFrame = currentFrame.callerFrame();
			if (currentFrame == null) {
				break;
			}
			stackItem = currentFrame.getStackItem();
		}

		switch (stackItem) {
			case null:
				return exception;
			case FilePos(_, file, line, _):
				for (index => item in exception.stack.asArray()) {
					switch (item) {
						case FilePos(_, file2, line2, _) if (skipping == 0 && file == file2 && line == line2):
							stack.push(item);
							skipping = 0;
						// TODO: this is silly
						case FilePos(Method("hxcoro.CoroRun", "run"), _) if (skipping == 1):
							skipping = 2;
						// this is a hack
						case FilePos(Method(_, "invokeResume"), _) if (skipping == 0):
							skipping = 1;
							localInsertIndex = index;
						case _:
							if (skipping != 1) {
								stack.push(item);
							}
					}
				}
			case _:
				return exception;
		}
		exception.stack = stack;
		insertIndex.value = localInsertIndex;
		#end
		return exception;
	}

	public function buildCallStack(cont:BaseContinuation<Any>):Void {
		#if js
		return;
		#end
		#if debug
		if (@:privateAccess cont._hx_startedException) {
			return;
		}

		final stackItem = cont.getStackItem();
		if (stackItem != null) {
			final idx = insertIndex.value;
			if (idx != null) {
				final stack = cont.error.stack.asArray();
				stack.insert(idx, stackItem);
				cont.error.stack = stack;
				insertIndex.value = idx + 1;
			}
		}
		#end
	}
}
