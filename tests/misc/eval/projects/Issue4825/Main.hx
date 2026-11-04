@:genericBuild(Macro.buildSomething())
class C<T> { }

@:eager typedef T = C<String>;

class Main {
    static var k:K; // Type not found : K
    public static function main() { }
}