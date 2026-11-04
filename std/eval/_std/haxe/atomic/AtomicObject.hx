package haxe.atomic;

private typedef AtomicObjectData<T:haxe.Constraints.ReferenceType> = Any;

extern abstract AtomicObject<T:haxe.Constraints.ReferenceType>(AtomicObjectData<T>) {
	public function new(value:T):Void;

	public function compareExchange(expected:T, replacement:T):T;

	public function exchange(value:T):T;

	public function load():T;

	public function store(value:T):T;
}
