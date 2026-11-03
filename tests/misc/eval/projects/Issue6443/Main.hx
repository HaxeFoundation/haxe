class Main {
	static function main() {
		var first = true;
		do {
			if (first) {
				first = false;
				continue;
			}
			break;
		} while (true);
		Sys.stderr().writeString("ok");
	}
}