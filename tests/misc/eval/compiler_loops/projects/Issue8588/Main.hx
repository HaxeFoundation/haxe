abstract Under<T>(T) {
	@:from public static function from<T>(v:T) return v;
}

class Main {
	static public function main() {
		var a1:Under<{}> = { };
	}
}