class Main {
    static function main() {
        js.Promise.resolve(0).then(function(c) test(c));
    }

    static function test(a:{test:String}) {}
}
