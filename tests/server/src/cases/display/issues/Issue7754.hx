package cases.display.issues;

class Issue7754 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				Foo.foo({-1-});
			}
		}
		extern class Foo {
			@:overload(function(?s:String):Void {})
			static function foo(?i:Int):Void;
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseSignatureHelp();
		var sigs = result.result.signatures;
		Assert.equals(2, sigs.length);
		Assert.equals('Null<String>', strType(sigs[0].args[0].t));
		Assert.equals('Null<Int>', strType(sigs[1].args[0].t));
	}
}