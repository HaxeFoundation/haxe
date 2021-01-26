package unit.issues;

@:keep
class Issue6208 extends unit.Test implements IBase implements IChild {

    function test() {
        t(Std.isOfType(this, IChild));
        t(Std.isOfType(this, IBase));
    }

    public function base() {}
    public function child() {}
}

@:keep interface IBase {
    function base():Void;
}

@:keep interface IChild extends IBase {
    function child():Void;
}