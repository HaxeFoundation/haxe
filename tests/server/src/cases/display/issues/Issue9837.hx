package cases.display.issues;

class Issue9837 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var m = new Main();
				m.example({-1-}
			}

			function new() {}

			macro function example(param:Float) {
				return macro null;
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.SignatureHelp, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: false
		});
		Assert.equals(1, result.signatures.length);
		Assert.equals(0, result.activeParameter);
		var args = result.signatures[0].args;
		Assert.equals(1, args.length);
		Assert.equals("param", args[0].name);
	}
}
