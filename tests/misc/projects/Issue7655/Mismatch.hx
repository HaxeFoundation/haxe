import js.Promise;

class Mismatch {
	static function main() {
		var p:Thenable<String> = new Promise<String>(null);
		p.then(x -> 10, e -> "");
	}
}
