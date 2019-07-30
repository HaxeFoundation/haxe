abstract Under<T>(T) {
	function new(v:T) {
		this = v;
	}

	@:from public static function from<T>(v:T) {
		return v;
	}
}

class Main {
	static public function main() {
		var a1:Under<{}> = { };
	}
}