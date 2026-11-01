package cases.display.issues;

class Issue10414 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var a = 1;
				var obj:{
					foo:Int,
					bar:Bool
				} = {
					foo:{-1-}
					bar: true
				}
			}
		}
	**/
	function test(_) {
		final completion = runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		assertHasCompletion(completion, item -> item.args.name == 'a');
		Assert.equals("", completion.filterString);
	}

	/**
		class Main {
			static function main() {
				var abc = 1;
				var obj:{
					foo:Int,
					bar:Bool
				} = {
					foo:{-1-}a{-2-}b{-3-}
					bar: true
				}
			}
		}
	**/
	function test2(_) {
		final completion = runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		assertHasCompletion(completion, item -> item.args.name == 'abc');
		Assert.equals("ab", completion.filterString);

		final completion = runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(2),
			wasAutoTriggered: true
		});
		assertHasCompletion(completion, item -> item.args.name == 'abc');
		Assert.equals("a", completion.filterString);

		final completion = runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(3),
			wasAutoTriggered: true
		});
		assertHasCompletion(completion, item -> item.args.name == 'abc');
		Assert.equals("ab", completion.filterString);
	}

	/**
		class Container {
			public function new() {};
		}
		class Main {
			static function main() {
				final rockC = addContainer();
				addSprite({
					container: {-1-}
					frame: ""
				});
			}
			static function addContainer():Container return new Container();
			static function addSprite(obj:{
				container:Container,
				frame:String
			}):Void {}
		}
	**/
	function test3(_) {
		final completion = runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		assertHasCompletion(completion, item -> item.args.name == 'rockC');
		Assert.equals("", completion.filterString);
	}

	/**
		class Container {
			public function new() {};
		}
		class Main {
			static function main() {
				final rockC = addContainer();
				addSprite({
					container: {-1-},
					frame: ""
				});
			}
			static function addContainer():Container return new Container();
			static function addSprite(obj:{
				container:Container,
				frame:String
			}):Void {}
		}
	**/
	function test4(_) {
		final completion = runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		assertHasCompletion(completion, item -> item.args.name == 'rockC');
		Assert.equals("", completion.filterString);
	}

	/**
		class Container {
			public function new() {};
		}
		class Main {
			static function main() {
				final rockC = addContainer();
				addSprite({
					container: rockC,
					container: {-1-}
					frame: ""
				});
			}
			static function addContainer():Container return new Container();
			static function addSprite(obj:{
				container:Container,
				frame:String
			}):Void {}
		}
	**/
	function test5(_) {
		final completion = runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		assertHasCompletion(completion, item -> item.args.name == 'rockC');
		Assert.equals("", completion.filterString);
	}
}