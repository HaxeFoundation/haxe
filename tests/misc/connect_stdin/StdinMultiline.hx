class StdinMultiline {
	static function main() {
		var lines = [];
		try {
			while (true) {
				lines.push(Sys.stdin().readLine());
			}
		} catch (_:haxe.io.Eof) {}
		Sys.println("Got " + lines.length + " lines: " + lines.join(", "));
	}
}
