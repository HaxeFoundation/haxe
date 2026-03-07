package haxe.atomic;

private typedef AtomicObjectData<T:{}> = Any;

extern abstract AtomicObject<T:{}>(AtomicObjectData<T>) {
	public function new(value:T):Void;

	public function compareExchange(expected:T, replacement:T):T;

	public function exchange(value:T):T;

	public function load():T;

	public function store(value:T):T;
}
