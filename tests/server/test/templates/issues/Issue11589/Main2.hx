function main() {
	foo("");
}

function foo<T>(v:T):Foo<T> return {foo:{bar:v}}
typedef Foo<T> = { foo : { bar : T } }
