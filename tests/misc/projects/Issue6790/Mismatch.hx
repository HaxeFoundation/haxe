import js.lib.Promise;

class Mismatch {
	static function main() {
		var p = new Promise<String>(null);
		p.then(x -> 10, e -> "");
	}
}
