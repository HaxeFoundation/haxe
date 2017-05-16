package unit.issues;

class Issue6281 extends unit.Test {
    public static var check(get, null):String;

    static function get_check():String {
        return (check == null ? check = 'foo' : check);
    }

    public function test() {
        eq('foo', check);
    }
}