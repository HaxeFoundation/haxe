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
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Float";
			case _: false;
		});

		result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Float";
			case _: false;
		});
	}

	/**
		abstract Foo1(Int) from {-1-}
	**/
	function test2(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
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
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Float";
			case _: false;
		});
	}
}
