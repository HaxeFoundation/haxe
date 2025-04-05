import haxe.Exception;

extern class BaseExtern {
	var field:Int;
	function new(i:Int):Void;
}

@:jsRequire('./externs.js', 'ExtendedExtern')
extern class ExtendedExtern extends BaseExtern {}

class HaxeClass extends ExtendedExtern {}

class Main {
	static function main() {
		var hx = new HaxeClass(999);
		if(hx.field != 999) {
			throw new Exception('Super constructor was not invoked');
		}
	}
}