class TestMakefile {
	static public function main() {
		Sys.setCwd("../");
		var mlFiles = [];
		FileSystemTools.mapFiles("./src", function(s) {
			mlFiles.push(s);
		}, ~/\.ml$/);

		var failure = false;
		var times = [];

		function makeWithTimer(name) {
			var start = haxe.Timer.stamp();
			var code = make();
			var time = haxe.Timer.stamp() - start;
			times.push({name: name, time: time});
			return code;
		}

		Sys.command("make", ["clean_haxe"]);
		makeWithTimer("initial");

		for (mlFile in mlFiles) {
			Sys.command("touch", [mlFile]);
			Sys.println('[START] $mlFile');
			var code = makeWithTimer(mlFile);
			if (code == 0) {
				Sys.println('[DONE] $mlFile');
			} else {
				Sys.println('[ERROR] $mlFile');
				failure = true;
				break;
			}
		}

		times.sort(function(t1, t2) return t1.time < t2.time ? -1 : 1);
		var total = 0.;
		for (time in times) {
			Sys.println('${time.name}: ${ghettoFormatTime(time.time)}');
			total += time.time;
		}
		Sys.println('Total: ${ghettoFormatTime(total)}');
		Sys.exit(failure ? 1 : 0);
	}

	static function make() {
		return Sys.command("make", ["LFLAGS=-c", "haxe"]);
	}

	static function ghettoFormatTime(f:Float) {
		var s = Std.string(f);
		return s.substr(0, s.indexOf(".") + 3);
	}
}

class FileSystemTools {
	static public function mapFiles(directory:String, f:String -> Void, ?match:EReg) {
		var workList = [directory];
		while (workList.length > 0) {
			var current = workList.shift();
			if (sys.FileSystem.isDirectory(current)) {
				var files = sys.FileSystem.readDirectory(current);
				for (file in files) {
					switch (file) {
						case "." | "..":
						case _: workList.push(haxe.io.Path.join([current, file]));
					}
				}
			} else {
				if (match.match == null || match.match(current)) {
					f(current);
				}
			}
		}
	}
}