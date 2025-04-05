abstract Under<T>(T) {
	@:from public static function from<T>(v) return (v:Under<T>);
}

class Main {
	static function main() {
		var a1:Under<{}> = {};
	}
}