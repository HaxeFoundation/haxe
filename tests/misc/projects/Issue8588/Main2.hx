abstract Under<T>(T) {
	function new(v:T) this = v;
	@:from public static function from<T>(v) return (v:Under<T>);
}

class Main {
	static function main() {
		var a1:Under<{}> = {};
	}
}