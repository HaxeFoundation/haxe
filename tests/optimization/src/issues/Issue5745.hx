package issues;

import TestJs.use;

class Issue5745 {
	@:js('
		var v = cat("filename");
		runProgram.apply(undefined, ["rm","filename"]);
		TestJs.use(v);
	')
    static function test() {
        var fn = 'filename';
        var v = Shell.cat(fn);
        Shell.runProgram(['rm', fn]);
        use(v);
    }
}

class Shell {
    public inline static function runProgram(args:Array<String>):Int {
        return js.Syntax.code('runProgram.apply(undefined, {0})', args);
    }
    public inline static function cat(v:String):String {
        return js.Syntax.code('cat({0})', v);
    }
}