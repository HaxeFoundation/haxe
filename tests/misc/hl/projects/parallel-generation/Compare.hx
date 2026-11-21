class Compare {
	static function main() {
		var args = Sys.args();
		var a = sys.io.File.getBytes(args[0]);
		for (file in args.slice(1)) {
			var b = sys.io.File.getBytes(file);
			if (a.compare(b) != 0) {
				Sys.println('${args[0]} and $file differ');
				Sys.exit(1);
			}
		}
	}
}
