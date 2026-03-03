package cases.display.issues;

class Issue7029 extends DisplayTestCase {
	/**
		class C implements {-1-}

		interface IFoo { }
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "IFoo";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C";
			case _: false;
		});
	}

	/**
		class C1 extends {-1-}

		class C2 { }
		interface IFoo { }
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C2";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "IFoo";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C1";
			case _: false;
		});
	}

	/**
		class C { }
		interface IFoo { }
		interface IFoo2 extends {-1-} { }
	**/
	function test3(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "IFoo";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "IFoo2";
			case _: false;
		});
	}

	/**
		typedef T1 = { };
		class C1 { }

		typedef T2 = {
			> {-1-}
		}
	**/
	function test4(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "T1";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C1";
			case _: false;
		});
	}

	/**
		typedef T1 = { };
		class C1 { }

		typedef T2 = {
			> T{-1-}
		}
	**/
	function test5(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "T1";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C1";
			case _: false;
		});
	}

	/**
		typedef T1 = { };
		typedef T2 = { };
		class C1 { }

		typedef T3 = {
			> T1,
			> {-1-}
		}
	**/
	function test6(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "T2";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C1";
			case _: false;
		});
	}

	/**
		interface I1 {}
		typedef T1 = {};
		enum E1 {}
		class C1 {
			public function new() { }
		}

		class C2 {
			static function main() {
				new{-1-}   {-2-}
			}

			public function new() { }
		}
	**/
	function test7(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C1";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C2";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "I1";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "T1";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "E1";
			case _: false;
		});
	}

	/**
		final class C1 { }
		class C2 extends {-1-} { }
	**/
	function test8(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C1";
			case _: false;
		});
	}

	/**
		final class C1 { }
		@:hack class C2 extends {-1-} { }
	**/
	function test9(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "C1";
			case _: false;
		});
	}
}
