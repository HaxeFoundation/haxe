class F {
    @:generic
    public static function make<T:haxe.Constraints.Constructible<()->Void>>() {
        return new T();
    }
}

class Main1 {
    static function main() {
        F.make();
    }
}