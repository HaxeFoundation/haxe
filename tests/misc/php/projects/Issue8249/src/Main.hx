interface Test {
    @:keep final test : String;
}

class Main implements Test {
    public final test : String = "ok";
    public function new() {}

    static function main() {}
}