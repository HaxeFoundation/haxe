package sys.thread;

import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Condition as NativeCondition;

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
	public function wait():Void {
		native.await();
	}

	public function signal():Void {
		native.signal();
	}

	public function broadcast():Void {
		native.signalAll();
	}
}
