class Main {
  static function main() {
    var a = new A();
    a[0] = "bar";
    var b = new B();
    b[0] = "bar";
  }
}


@:coreType
@:arrayAccess
abstract A {
  inline public function new() {
    this = untyped __js__("{}");
  }
}

typedef TB = B;

@:arrayAccess
abstract B(TB) {
  inline public function new() {
    this = untyped __js__("{}");
  }
}