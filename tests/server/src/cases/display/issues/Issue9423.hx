package cases.display.issues;

import haxe.display.Protocol;
using Lambda;

class Issue9423 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				Mod9423{-1-}
			}
		}
	**/
	function test(_) {
		vfs.putContent("Mod9423.hx", getTemplate("issues/Issue9423/Mod9423.hx"));
		vfs.putContent("Mod9423.whatever.hx", getTemplate("issues/Issue9423/Mod9423.whatever.hx"));
		runHaxeJson(["-cp", "."], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: false
		});
		var result = parseCompletion().result;
		final l = result.items.filter(di -> switch [di.kind, di.args] {
			case [Type, args]: args.path.typeName == "Mod9423";
			case _: false;
		});
		Assert.equals(1, l.length);
	}
}
