class C {
    public function new(_) {}
    public function f() {}
}

abstract A(C) {
    public function new() {
        this = new C(this.f);
    }
}

class Main1 {
    static function main() {
    }
}