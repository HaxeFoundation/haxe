enum E {
    CTor(i:Int);
}

class Main {
    static public function main() {
        expectE(CTor(foo));
    }

    static function expectE(e:E) { }
}