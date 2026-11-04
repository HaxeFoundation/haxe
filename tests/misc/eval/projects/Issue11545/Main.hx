class Main {
	static function main() {
		var name = new ImageName("abc");
		trace(name);
	}
}

abstract ImageName(String) {
	public function new(name:String) {
		this = name;
	}
}
