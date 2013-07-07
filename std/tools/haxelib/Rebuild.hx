package tools.haxelib;

import sys.FileSystem;
import sys.io.Process;

class Rebuild {
	static function run(cmd:String, ?msg:String = '', ?args:Array<String>) {
		if (args == null) args = [];
		var p = new Process(cmd, args);
		if (p.exitCode() != 0) 
			throw 'Error $msg:' + p.stderr.readAll().toString();
	}
	static function main() try {
		switch Sys.systemName() {
			case 'Windows':
			case os: throw 'Wrong OS. Expected Windows but detected $os';
		}
		var haxepath = Sys.getEnv("HAXEPATH");
		var file = '$haxepath/haxelib.n';
		
		run('haxe', 'rebuilding haxelib', [
			'-neko', file, 
			'-lib', 'haxelib_client', 
			'-main', 'tools.haxelib.Main', 
		]);
		
		run('nekotools', 'booting haxe', ['boot', file]);
		FileSystem.deleteFile(file);
		FileSystem.deleteFile('update.hxml');
		Sys.println('Update successful');
	}
	catch (e:Dynamic) {
		Sys.println(Std.string(e));
	}
}