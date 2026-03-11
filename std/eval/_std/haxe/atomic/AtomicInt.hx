package haxe.atomic;

private typedef AtomicIntData = Any;

extern abstract AtomicInt(AtomicIntData) {
	public function new(value:Int):Void;

	public function add(b:Int):Int;

	public function sub(b:Int):Int;

	public function and(b:Int):Int;

	public function or(b:Int):Int;

	public function xor(b:Int):Int;

	public function compareExchange(expected:Int, replacement:Int):Int;

	public function exchange(value:Int):Int;

	public function load():Int;

	public function store(value:Int):Int;
}
