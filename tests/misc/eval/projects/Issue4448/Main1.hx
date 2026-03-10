abstract A(Int) {
    function f() {}

    var v(get,never):Int;
    function get_v() return this;

    var v2(get,never):Int;
    inline function get_v2() return this;

    static function e() {
        f(); // Not enough arguments, expected this:Int - should be just unavailable
        v; // generates _$Main_A_$Impl_$.get_v();
        v2; // Invalid abstract implementation function
    }
}

class Main {
	static function main() {

	}
}