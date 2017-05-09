package unit.issues;

class Issue5155 extends unit.Test {
    public function toString () {
        var full = Type.getClassName(Type.getClass(this));
        var short = full.split (".").pop ();
        return "[object " + short + "]";
    }

    function test() {
        unspec(() -> Type.getClass(this));
    }
}