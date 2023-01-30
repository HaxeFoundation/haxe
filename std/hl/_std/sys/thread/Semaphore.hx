package sys.thread;

#if doc_gen
@:coreApi extern class Semaphore {
	function new(value:Int):Void;
	function acquire():Void;
	function tryAcquire(?timeout:Float):Bool;
	function release():Void;
}
#else
abstract Semaphore(hl.Abstract<"hl_semaphore">) {
	public function new(value:Int):Void {
		this = alloc(value);
	}

	@:hlNative("std", "semaphore_acquire")
	public function acquire():Void {}

	@:hlNative("std", "semaphore_release")
	public function release():Void {}

	@:hlNative("std", "semaphore_try_acquire")
	public function tryAcquire(?timeout:Float):Bool {
		return false;
	}

	@:hlNative("std", "semaphore_alloc")
	static function alloc(value:Int):hl.Abstract<"hl_semaphore"> {
		return null;
	}
}
#end
