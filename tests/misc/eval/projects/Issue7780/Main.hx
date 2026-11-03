import haxe.PosInfos;

typedef Struct = {
	final foo:Int;
}

class Main {
	static function main() {
		foo(({foo: 0} : Struct));
	}

	static function foo(struct) {
		trace(struct.foo);
	}
}


class Main2 extends Foo {
	static function main() {
		Main2.trace = (d:Dynamic, infos:haxe.PosInfos) -> {};
	}
	function a(v:String) {}
	function b(v:String = 'hey') {}
	function c(v:String) {}
	function d(?v:String) {}

	public static dynamic function trace(v:Dynamic, ?infos:PosInfos):Void {}
}

abstract class Foo {
	abstract function a(val:String = 'hey'):Void;
	abstract function b(val:String):Void;
	abstract function c(?val:String):Void;
	abstract function d(val:Int):Void;
}

class Main3 implements IMain {
	public function foo(i:Int = 1):Void {}
}

class MainOk implements IMain {
	// should pass
	public function foo(?i:Int = 1):Void {}
}

interface IMain {
	function foo(?i:Int):Void;
}
