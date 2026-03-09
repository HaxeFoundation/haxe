class Main {
    static macro function m() {
        return macro @:pos(haxe.macro.Context.currentPos()) @:noPrivateAccess A.a;
    }

	static function main() {
        m();
	}
}

class A {
    static var a:Int;
}
