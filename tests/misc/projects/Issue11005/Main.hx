function main() {}

abstract Foo(String) {
	public inline function new(str:String)
		this = str;
}

inline final foo = new Foo("foo");
