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
		final seconds = if (!(t > 0)) 0.0 else if (t > 1e9) 1e9 else t;
		return native.await(haxe.Int64.fromFloat(seconds * 1000000000), TimeUnit.NANOSECONDS);
	}

	public function signal():Void {
		native.signal();
	}

	public function broadcast():Void {
		native.signalAll();
	}
}
