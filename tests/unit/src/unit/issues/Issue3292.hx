package unit.issues;

private class Component {
    public static var TYPE : String = "type";
    public function doSomething() { return TYPE; };
    public function new() { };
}

private class A {
    var _components : Map<String, Component>;

    public function new() {
        _components = new Map<String, Component>();
        _components.set( Component.TYPE, new Component() );
    }

    @:generic public function get<T:(Component)>( classType : Class<T> ) : T {
        return cast _components.get( Reflect.field( classType, "TYPE" ) );
    }
}

private class B extends A {
    public function new() {
        super();
    }
}

class Issue3292 extends Test {
	function test() {
        var a : A = new A();
        eq("type", a.get( Component ).doSomething());

        var b : B = new B();
        eq("type", b.get( Component ).doSomething());
	}
}