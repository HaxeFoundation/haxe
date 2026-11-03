package cases.display.issues;

class Issue8180 extends DisplayTestCase {
	/**
		class Main {
			#if !macro
			static function main() {
				f({-1-}
			}
			#end

			static macro function f(e) {
				switch e {
					case {expr: EDisplay(macro null, DKMarked), pos: p}:
						return {pos: p, expr: EDisplay(macro {x:1}, DKDot)};
					case _:
						return macro null;
				}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "x" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
