class Main {
    static function main() {
        f(null);
    }

    static function f(t:Token) {
        $type(t);
        $type(t.type);
        if (t.type || t.type == 42)
            throw "WAT";
    }
}

abstract Token(Array<Dynamic>) {
    public var type(get,never):String;
    inline function get_type() return this[0];
}