class Main {
	static function main() {
		var a = [1, 2, 3];
		var result:Dynamic = a.remove(2);
		if (result != "Array.remove") {
			throw 'Array from the added class path was not used, remove returned $result';
		}
	}
}
