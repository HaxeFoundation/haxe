class C {
	public var f = 5;
	public function new(x:Int = this.f) {}
}

class Main {
	static function main() {
		new C();
	}
}
