package cases.display.issues;

class Issue7327 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some(v):{-1-} {-2-}
					case None:
				}
			}
		}
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "v");

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "v");
	}

	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some(v):{-1-} {-2-}
				{-3-}}{-4-}
			}
		}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "v");

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "v");

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(3), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "v");

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(4), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasNoCompletion(result, item -> item.kind == Local && item.args.name == "v");
	}

	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some(v):{-1-}
					default: {-2-}
				}
			}
		}
	**/
	function test3(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "v");

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasNoCompletion(result, item -> item.kind == Local && item.args.name == "v");
	}

	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some(v):{-1-}
	**/
	function test4(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "v");
	}
}
