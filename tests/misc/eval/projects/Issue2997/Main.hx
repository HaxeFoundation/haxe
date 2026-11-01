enum abstract MyEnum(Int) {
    var A = 1;
    var B = 2;
    var C = 3;
}

class Main {
    static function main() {
        var a = A;
        var b = MyEnum.B;
    }
}