package sys.thread;

@:coreApi class Semaphore {
	final native:eval.luv.Semaphore;

	public function new(value:Int):Void {
		native = eval.luv.Semaphore.init(value).resolve();
		eval.vm.Gc.finalise(destroy, this);
	}

    static function destroy(sem:Semaphore):Void {
        sem.native.destroy();
    }

	public function acquire():Void {
		native.wait();
	}

	public function tryAcquire(?timeout:Float):Bool {
		if (timeout == null) {
			return native.tryWait().isOk();
		} else {
			var t = Sys.time() + timeout;
			while (Sys.time() < t) {
				if (native.tryWait().isOk()) {
					return true;
				}
			}
			return false;
		}
	}

	public function release():Void {
		native.post();
	}
}
