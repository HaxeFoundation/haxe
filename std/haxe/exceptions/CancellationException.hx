package haxe.exceptions;

class CancellationException extends CoroutineException {
	public function new() {
		super("Cancellation");
	}
}
