@:genericBuild(Macro.build())
class C<T> {}

class Main {
    static function main() {
        var c = new C<Int>();
    }
}