package sys.thread;

import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Condition as NativeCondition;
import java.util.concurrent.TimeUnit;

@:access(sys.thread.Mutex)
@:coreApi
@:native('haxe.java.vm.Condition')
class Condition {
	final lock:ReentrantLock;
	final native:NativeCondition;

	public function new():Void {
		this.lock = new ReentrantLock();
		this.native = lock.newCondition();
	}

	public function acquire():Void {
		lock.lock();
	}

	public function tryAcquire():Bool {
		return this.lock.tryLock();
	}

	public function release():Void {
		lock.unlock();
	}

	// without the @:native, you get "java.lang.VerifyError: class sys.thread.Condition overrides final method java.lang.Object.wait()V" on jvm
	// and "wait() in Condition cannot override wait() in Object" from javac

	@:native("waitOn")
	public function wait(?timeout:Float):Bool {
		if (timeout == null) {
			native.await();
			return true;
		}
		final t:Float = timeout;
		// Int64.fromFloat only accepts values up to 2^53, which is about 104 days in nanoseconds
		final maxSeconds = 9e6;
		final capped = t > maxSeconds;
		final seconds = if (!(t > 0)) 0.0 else if (capped) maxSeconds else t;
		final signaled = native.await(haxe.Int64.fromFloat(seconds * 1000000000), TimeUnit.NANOSECONDS);
		// a capped wait did not reach the requested timeout, so it counts as a spurious wakeup
		return signaled || capped;
	}

	public function signal():Void {
		native.signal();
	}

	public function broadcast():Void {
		native.signalAll();
	}
}
