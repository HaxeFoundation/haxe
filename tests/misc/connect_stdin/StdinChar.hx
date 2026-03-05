class StdinChar {
	static function main() {
		final code = Sys.getChar(false);
		Sys.println("Got: " + String.fromCharCode(code));
	}
}
