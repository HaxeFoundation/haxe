import utest.Runner;
import utest.ui.Report;

import sys.FileSystem;

class Main {
	public static function main():Void {
		if (FileSystem.exists("resources-rw")) {
			function walk(path:String):Void {
				for (f in FileSystem.readDirectory(path)) {
					if (FileSystem.isDirectory('$path/$f')) {
						walk('$path/$f');
						FileSystem.deleteDirectory('$path/$f');
					} else {
						FileSystem.deleteFile('$path/$f');
					}
				}
			}
			walk("resources-rw");
		} else {
			FileSystem.createDirectory("resources-rw");
		}
		var runner = new Runner();
		runner.addCases(test);
		runner.onTestStart.add(test -> trace("running", Type.getClassName(Type.getClass(test.fixture.target)), test.fixture.method));
		Report.create(runner);
		runner.run();
	}
}
