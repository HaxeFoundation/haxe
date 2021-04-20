class WithAbstractCast {
	static function main() {
		var foo:Vector<Int> = null;
		var bar:Vector<String> = Vector.fromIterable(foo);
	}
}

abstract Vector<T>(Array<T>) {
	@:from static public function fromVector<T1, R1:T1>(v:Vector<R1>):Vector<T1>
		return cast v;

	static public function fromIterable<T2, R2:T2>(v:Iterable<R2>):Vector<T2>
		return null;

	@:to public function toArray()
		return this.copy();
}