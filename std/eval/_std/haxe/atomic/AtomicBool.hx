package haxe.atomic;

#if !(target.atomics || core_api)
#error "Atomic operations are not supported on this target!"
#end

private typedef AtomicBoolData = Any;

extern abstract AtomicBool(AtomicBoolData) {
	public function new(value:Bool):Void;

	public function compareExchange(expected:Bool, replacement:Bool):Bool;

	public function exchange(value:Bool):Bool;

	public function load():Bool;

	public function store(value:Bool):Bool;
}