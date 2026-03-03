package cases.display.issues;

class Issue5767 extends DisplayTestCase {
	/**
		class Main {
			static function doStuff(options:{a:Float, b:String}) {}

			static function main () {
				doStuff({ a: 0.5, {-1-}
			}
		}
	**/
	function testGama11(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "b" && item.args.field.type.args.path.typeName == "String";
			case _: false;
		});
	}

	/**
		class Main {
			static function doStuff(options:{a:Float, b:String}) {}

			static function main () {
				doStuff({ a: 0.5, {-1-}});
			}
		}
	**/
	function testGama11Intact(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "b" && item.args.field.type.args.path.typeName == "String";
			case _: false;
		});
	}

	/**
		typedef T = {
			a:String,
			b:Int,
			c:Bool
		}

		class Main {
			static function main() {
				var c:T = {a:"foo", {-1-}
			}
		}
	**/
	function testOrder(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "b" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "c" && item.args.field.type.args.path.typeName == "Bool";
			case _: false;
		});
	}

	/**
		typedef T1 = {
			a:String,
			b:T2
		}

		typedef T2 = {
			a:Bool,
			b:Int
		}

		class Main {
			static function main() {
				var c:T1 = {a:"foo", b: {b:1, {-1-}
			}
		}
	**/
	function testNested(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "a" && item.args.field.type.args.path.typeName == "Bool";
			case _: false;
		});
	}

	/**
		typedef T1<T> = {
			a:String,
			b:T2<T>
		}

		typedef T2<T> = {
			a:T,
			b:Int
		}

		class Main {
			static function main() {
				var c:T1<Bool> = { b: { b:1, {-1-}
			}
		}
	**/
	function testFirstArg(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "a" && item.args.field.type.args.path.typeName == "Bool";
			case _: false;
		});
	}

	/**
		typedef T1<T> = {
			a:String,
			b:String,
			c:String
		}

		class Main {
			static function main() {
				var c:T1 = { a: "foo",{-1-} b: "bar" };
			}
		}
	**/
	function testIntact(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "c" && item.args.field.type.args.path.typeName == "String";
			case _: false;
		});
	}
}
