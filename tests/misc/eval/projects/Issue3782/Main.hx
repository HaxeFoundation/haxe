abstract Ab1(Ab2) from Ab2 {}
abstract Ab2(Ab1) from Ab1 {}

class Main {
    static function main() {
		var a:Ab1 = null;
		var b:Ab2 = a;
    }
}