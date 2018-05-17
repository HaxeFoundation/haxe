package unit.issues;

private interface InterfaceA<T> { }

private abstract AbstractC<T>(ClassC<T>) {
    public function new() this = new ClassC();
}

private class ClassC<T> implements InterfaceA<T> {

    public var foo(get, never):Null<T>;
    function get_foo():Null<T> return null;

    public function new() { }
}

private interface EmptyGenericInterface<T> {
}

private class ClassWithNullableField<T> implements EmptyGenericInterface<T> {
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