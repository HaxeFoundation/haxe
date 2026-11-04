class Main {
	static public function main() {}
}

abstract Val<T>(T) from Int {
	public inline function new(v:T)
		this = v;
	public inline function value():T return this;
}