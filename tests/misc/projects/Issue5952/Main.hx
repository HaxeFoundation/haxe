class Main {
    public static function foo<T>(v:T):T return v;

    public static function main():Void {
        var a = foo;
        doSwitch(a);
    }

    public static function doSwitch<A, T:A->A>(a:T):Void {
        switch (a){
            case _.bind("asdf", "foo") => b: trace(b());
        }
    }
}