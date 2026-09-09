class Main {
	static function main() {
		function foo<T>(arr:Array<T>) sortDesc(arr);

		foo([""]);
		foo([42]);
	}

	@:generic
	public inline static function sortDesc<T>(array : Array<T>) {}
}
