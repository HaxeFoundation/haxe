@:genericBuild(Macro.build())
class Foo<T> {

}

class Main {
	static function main() {
		var a:Foo<String>;
		var b:Foo<Int>;
	}
}