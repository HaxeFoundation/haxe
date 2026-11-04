@:semantics(value)
@:cpp.ValueType
extern class Struct {
    function new();
    var a:Int;
}

@:headerCode("struct Struct { int a; Struct() {} };")
class Compile2 {
    static function main() {
        final struct : cpp.RawPointer<Struct> = cast new Struct();
    }
}