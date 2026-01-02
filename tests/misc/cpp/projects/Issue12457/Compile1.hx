@:semantics(value)
@:cpp.ValueType
extern class Struct {
    function new();
    var a:Int;
}

@:headerCode("struct Struct { int a; Struct() {} };")
class Compile1 {
    static function main() {
        final struct : cpp.Star<Struct> = cast new Struct();
    }
}