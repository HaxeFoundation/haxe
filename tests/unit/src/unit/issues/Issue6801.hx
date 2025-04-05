package unit.issues;

import haxe.Json;
import haxe.format.JsonPrinter;

class Issue6801 extends unit.Test {
	function test() {
		var o = new Child();
        var json = haxe.format.JsonPrinter.print(o);

        var expected = {c:'hello', c2:true, p:1, p2:0};
        var actual = Json.parse(json);
        eq(Reflect.fields(expected).length, Reflect.fields(actual).length);
        eq(expected.c, actual.c);
        eq(expected.c2, actual.c2);
        eq(expected.p, actual.p);
        eq(expected.p2, actual.p2);
	}
}

@:keep
private class Parent {
    public var p:Int = 1;
    public function new() {}
}

@:keep
private class Child extends Parent{
    var c:String = 'hello';

    public var prop(get,set):Int;
    function get_prop() return 0;
    function set_prop(v) return v;

    public var c2(default,set):Bool = true;
    function set_c2(v) return c2 = v;

    @:isVar var p2(get,set):Int = 0;
    function get_p2() return p2;
    function set_p2(v) return p2 = v;

}