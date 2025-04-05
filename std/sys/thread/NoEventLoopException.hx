package sys.thread;

import haxe.Exception;

class NoEventLoopException extends Exception {
	public function new(msg:String = 'Event loop is not available. Refer to sys.thread.Thread.runWithEventLoop.', ?previous:Exception) {
		super(msg, previous);
	}
}