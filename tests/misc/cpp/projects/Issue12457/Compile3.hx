@:semantics(value)
@:cpp.ValueType
extern class Struct {
    function new();
    var a:Int;
}

@:headerCode("struct Struct { int a; Struct() {} };")
class Compile3 {
    static function main() {
        final struct : cpp.Pointer<Struct> = cast new Struct();
    }
}