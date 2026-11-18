class Compare {
	static function main() {
		for (ext in [".js", ".js.map"]) {
			var parallel = sys.io.File.getContent("bin/parallel" + ext);
			var sequential = sys.io.File.getContent("bin/sequential" + ext).split("sequential.js").join("parallel.js");
			if (parallel != sequential)
				throw 'bin/parallel$ext differs from bin/sequential$ext';
		}
	}
}
