class Main {
    static function main() {
        throw invalidChar(65);
    }

    static public function invalidChar(_) {
        throw "error";
    }
}