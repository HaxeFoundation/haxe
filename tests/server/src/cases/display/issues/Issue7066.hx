package cases.display.issues;

class Issue7066 extends DisplayTestCase {
	/**
		typedef Struct = {
			?fieldA:Int,
			?fieldB:String
		}

		class Main {
			static function main() {}
			function foo():Struct {
				return {
					fieldA: 5,
					{-1-}
				};
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.equals(1, result.items.length);
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "fieldB" && item.args.field.kind.kind == FVar;
			case _: false;
		});
	}
}
