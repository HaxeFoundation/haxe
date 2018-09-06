package unit.issues;

private abstract Foo (String) {
    public function new (s) {this = s;}
    public function getFoo ():String return this;
}

private abstract Bar ({a:Foo, b:Foo}) {
    public  function new (s) {this = s;}
    public function getBar ():{a:Foo, b:Foo} return this;
}

class Issue7367 extends unit.Test {
	function test() {
		var x = new Bar({a:new Foo("hello"), b: new Foo("hello2")});
        var s = switch x {
            case _.getBar() => {a:_.getFoo() => s1, b:_.getFoo() => s2}: s1+s2;
        }
        eq("hellohello2", s);
	}
}