package cases.display.issues;

class Issue7137 extends DisplayTestCase {
	/**
		abstract Foo1(Int) from {-1-} {}
		abstract Foo2(Int) to {-2-} {}

		class Main {
			static function main() {}
		}
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Float";
			case _: false;
		});

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Float";
			case _: false;
		});
	}

	/**
		abstract Foo1(Int) from {-1-}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Float";
			case _: false;
		});
	}

	/**
		abstract Foo1(Int) from {-1-} {
			public function new() { }
		}
	**/
	function test3(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Float";
			case _: false;
		});
	}
}
