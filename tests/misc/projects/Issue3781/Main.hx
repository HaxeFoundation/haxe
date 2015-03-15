enum E {
    E1<T:String>(v:T);
}

abstract Ab(String) to String {}

class Main<T:Ab> {
    static function main() {
    }

    function doStuff(v:T) {
        E1(v);
    }
}
