package cases.display.issues;

class Issue8660 extends DisplayTestCase {
	/**
		import haxe.Json as Jason;

		class Main {
			static function main() {
				var j:Ja{-1-}son;
				Ja{-2-}son;
			}
		}
	**/
	function test(_) {
		var typeHint = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		var expr = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		Assert.isTrue(typeHint != null && typeHint.length > 0);
		Assert.isTrue(expr != null && expr.length > 0);
		Assert.isTrue(StringTools.endsWith(haxe.io.Path.normalize(typeHint[0].file.toString()), "haxe/Json.hx"));
		Assert.equals(typeHint[0].file.toString(), expr[0].file.toString());
		Assert.same(typeHint[0].range, expr[0].range);
	}
}
