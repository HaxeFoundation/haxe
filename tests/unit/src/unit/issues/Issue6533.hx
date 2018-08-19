package unit.issues;

@:keep private interface InterfaceA<T> { }

@:keep private abstract AbstractC<T>(ClassC<T>) {
    public function new() this = new ClassC();
}

@:keep private class ClassC<T> implements InterfaceA<T> {

    public var foo(get, never):Null<T>;
    function get_foo():Null<T> return null;

    public var bar:Null<T>;

    public function new() { }
}

@:keep private interface EmptyGenericInterface<T> {
}

@:keep private class ClassWithNullableField<T> implements EmptyGenericInterface<T> {
	public var nullableField:Null<T>;

	public function new() {
	}
}

class Issue6533 extends Test {
    public function test():Void {
        var example:ClassWithNullableField<Int> = new ClassWithNullableField<Int>();
        example.nullableField = 42;
        var c = new AbstractC();
    }
}