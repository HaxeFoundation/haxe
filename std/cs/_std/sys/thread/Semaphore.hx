package sys.thread;

@:coreApi
class Semaphore {
	final native:SemaphoreSlim;

	public function new(value:Int):Void {
		this.native = new SemaphoreSlim(value);
	}

	public function acquire():Void {
		native.Wait();
	}

	public function tryAcquire(?timeout:Float):Bool {
		return native.Wait(timeout == null ? 0 : Std.int(timeout * 1000));
	}

	public function release():Void {
		native.Release();
	}
}

// doesn't seem to be in the dlls shipped with hxcs?

@:native("System.Threading.SemaphoreSlim")
private extern class SemaphoreSlim implements cs.system.IDisposable {
	overload function new(value:Int):Void;
	overload function new(value:Int, maximum:Int):Void;

	function Release():Void;
	overload function Dispose():Void;
	overload function Dispose(b:Bool):Void;
	overload function Wait():Void;
	overload function Wait(millisecondsTimeout:Int):Bool;
}
