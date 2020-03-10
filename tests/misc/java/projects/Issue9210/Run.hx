class Run {
	static function main() {
		var separator = if (Sys.systemName() == "Windows") ";" else ":";
		Sys.exit(Sys.command('java -cp "bin/*${separator}bin" Main'));
	}
}
