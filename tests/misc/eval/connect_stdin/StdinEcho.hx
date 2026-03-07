class StdinEcho {
	static function main() {
		var line = Sys.stdin().readLine();
		Sys.println("Got: " + line);
	}
}
