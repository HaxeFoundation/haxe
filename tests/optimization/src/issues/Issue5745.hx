package issues;

import TestJs.use;

class Issue5745 {
	@:js('
		var fn = "filename";
		var v = cat(fn);
		runProgram.apply(undefined, ["rm",fn]);
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
        return untyped __js__('runProgram.apply(undefined, {0})', args);
    }
    public inline static function cat(v:String):String {
        return untyped __js__('cat({0})', v);
    }
}