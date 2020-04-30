class Arr<T> {
	var get:T;

	inline function set(val:T):Void {}

	@:keep
	public function map<S>(f:T->S)
		(null:Arr<S>).set(f(get));
}