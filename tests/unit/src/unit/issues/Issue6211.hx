package unit.issues;

class Issue6211 extends unit.Test {

    function test() {
        var d:Dynamic = this;
        t(Reflect.isFunction(d.test));
    }
}