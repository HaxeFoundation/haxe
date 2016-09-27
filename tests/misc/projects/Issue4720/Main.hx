class Main {
    static function main() {
        // deprecating type objects work
        MyClass;
        MyInterface;
        MyEnum;
        // MyAbstract; // this compiles when analyzer=yes, but is another issue
        TClass;
        TInterface;
        TEnum;
        // TAbstract;  // this compiles when analyzer=yes, but is another issue
        // TAnon; // this won't and shouldn't compile anyway

        // deprecating types instantiated work
        new MyClass();
        MyEnum.None;
        new MyAbstract(null); // but doesn't work on abstracts...
        new TClass();
        TEnum.None;
        new TAbstract(null); // neither

        // some physical access to deprecated API
        deprecatedField;
        deprecatedFunc();
        deprecatedProperty; // this won't show warnings
        var x = deprecatedProperty; // this also
        deprecatedGetSet; // however this will
        var x = deprecatedGetSet; // this also
    }

    // deprecating fields work
    @:deprecated static var deprecatedField:String;
    @:deprecated static function deprecatedFunc() return;

    // ... however deprecating getters and setters have some gotcha
    @:deprecated static var deprecatedProperty(get, set):String;
    static function get_deprecatedProperty():String return "0";
    static function set_deprecatedProperty(value):String return "0";

    static var deprecatedGetSet(get, set):String;
    @:deprecated static function get_deprecatedGetSet():String return "0";
    @:deprecated static function set_deprecatedGetSet(value):String return "0";
}

@:deprecated
class MyClass { public function new() {} }

@:deprecated
interface MyInterface { }

@:deprecated
enum MyEnum { None; }

@:deprecated
abstract MyAbstract(String) { public function new(value:String) this = value; }

@:deprecated
typedef TClass = MyClass;

@:deprecated
typedef TInterface = MyInterface;

@:deprecated
typedef TEnum = MyEnum;

@:deprecated
typedef TAbstract = MyAbstract;

@:deprecated
typedef TAnon = { a:String };