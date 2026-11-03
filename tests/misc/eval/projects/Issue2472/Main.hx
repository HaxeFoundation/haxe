class Main {
	static function main() { }
}

class A<O:{id:String}> {
	public var o:O;
	public function new(o:O) {
		this.o = o;
	}
}

class B<O> extends A<O> {
	public function new(o) {
		super(o);
	}
}