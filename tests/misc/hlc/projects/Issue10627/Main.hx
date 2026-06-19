extern class ExtraFoo {
	static function foobar(i : Int) : Void;
}

/*
class ExtraFoo {
	static public function foobar(i : Int) {
		trace(i);
	}
}
*/

class Main {
    static public function main():Void {
		ExtraFoo.foobar(1);
    }
}
