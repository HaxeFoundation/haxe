class Container<T> {
	public function new() {}

	public function make(value:T) {
		var x:{a:T, b:{x:Int}} = {a: value, b: {x: 0}};
		return x;
	}
}
