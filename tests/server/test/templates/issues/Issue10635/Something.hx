class Something {
	@:generic
	static public function append<T>(a:Array<T>, b:Array<T>) {
		for (e in b) {
			a.push(e);
		}
		return a;
	}
}
