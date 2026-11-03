class Res {
    public static var name(get,never) : String;
    @:extern static inline function get_name() {
        return "OK";
    }
}

class Main {

    static function main() {
        trace(Res.name);
    }
}