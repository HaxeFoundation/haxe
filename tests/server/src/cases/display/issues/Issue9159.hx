package cases.display.issues;

class Issue9159 extends DisplayTestCase {
	/**
		@:structInit
		class CustomConstructor {
			public var nope1:String;
			public function new(x:Int = 0) {}
			public function nope2() {}
		}

		@:structInit
		class AutoConstructor {
			public var y:Float;
			public function nope() {}
		}

		class Main {
			static function main() {
				var a:CustomConstructor = {-1-}{};
				var b:AutoConstructor = {-2-}{};
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseCompletion().result;
		Assert.equals(1, result.items.length);
		Assert.equals('x', result.items[0].args.field.name);

		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(2),
			wasAutoTriggered: true
		});
		var result = parseCompletion().result;
		Assert.equals(1, result.items.length);
		Assert.equals('y', result.items[0].args.field.name);
	}
}