package neko;

enum ThreadHandle {
}

class Lock {
	var l : Void;
	public function new() {
		l = lock_create();
	}
	public function wait( ?timeout : Float ) : Bool {
		return lock_wait(l,timeout);
	}
	public function release() {
		lock_release(l);
	}
	static var lock_create = neko.Lib.load("std","lock_create",0);
	static var lock_release = neko.Lib.load("std","lock_release",1);
	static var lock_wait = neko.Lib.load("std","lock_wait",2);
}

class Thread {

	var handle : ThreadHandle;

	function new(h) {
		handle = h;
	}

	public function sendMessage( msg : Dynamic ) {
		thread_send(handle,msg);
	}

	public static function current() {
		return new Thread(thread_current());
	}

	public static function create<T>( callb : T -> Void, param : T ) {
		return new Thread(thread_create(callb,param));
	}

	public static function readMessage( block : Bool ) : Dynamic {
		return thread_read_message(block);
	}

	static var thread_create = neko.Lib.load("std","thread_create",2);
	static var thread_current = neko.Lib.load("std","thread_current",0);
	static var thread_send = neko.Lib.load("std","thread_send",2);
	static var thread_read_message = neko.Lib.load("std","thread_read_message",1);

}
