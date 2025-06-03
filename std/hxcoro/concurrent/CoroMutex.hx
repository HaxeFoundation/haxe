package hxcoro.concurrent;

class CoroMutex extends CoroSemaphore {
	public function new() {
		super(1);
	}
}
