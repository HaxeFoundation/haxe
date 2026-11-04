class StdinChar {
	static function main() {
		final code = Sys.getChar(false);
		final repr = if (code == 10) "\\n" else String.fromCharCode(code);
		Sys.println("Got: " + repr);
	}
}
