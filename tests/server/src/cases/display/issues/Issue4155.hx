package cases.display.issues;

class Issue4155 extends DisplayTestCase {
	/**
		class Main {
			macro static function m(e) {
				trace(haxe.macro.ExprTools.toString(e));
				return switch (e) {
					case macro (null:{
						function dummy():Void {
							$expr;
						}
					}):
						expr;
					case _: throw "invalid input";
				}
			}

			@:debug.display
			static function main() {
				var v = m((null:{
					function dummy():Void {
						"foo".{-1-}
					}
				}));
				trace(v);
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "length" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
