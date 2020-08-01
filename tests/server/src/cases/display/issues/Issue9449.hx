package cases.display.issues;

class Issue9449 extends DisplayTestCase {
	/**
		class C {
			public var x:Int;
			public function new() {}
		}

		function main() {
			Macro.f(new C(), {{-1-}});
		}
	**/
	function test(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue9449/Macro.hx"));
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseCompletion().result;
		Assert.equals(1, result.items.length);
		Assert.equals('x', result.items[0].args.field.name);
	}
}
