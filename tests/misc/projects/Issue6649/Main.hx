class Main {
	public static function main() {
		var v:Any = null;
		constraint(v);

		var a:Array<Int> = [1, 2, 3];
		param(a);
	}

	static function constraint<T:Main>(v:T) {}

	static function param(a:Array<Any>) {}
}
