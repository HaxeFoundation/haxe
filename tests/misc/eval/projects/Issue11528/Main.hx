class MyClass<T> {
	public static var Null = new MyClass<T>();

	public function new() {}
}

function main() {
	trace(MyClass.Null);
}