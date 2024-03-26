function main() {}

typedef Foo<T> = { foo : { bar : T } }

typedef Bar<T> = {
	function foo( elements : Array<{ value : T }> ) : Void;
}
