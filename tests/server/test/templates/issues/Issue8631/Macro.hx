using sys.FileSystem;
using sys.io.File;


 class Macro {
	static var called:Bool = false;
	static inline var FILE = 'counter.txt';

 	static public function call() {
		if(called) return;
		called = true;
		var cnt = FILE.exists() ? Std.parseInt(FILE.getContent()) + 1 : 1;
		FILE.saveContent('$cnt');
	}
}