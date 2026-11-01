package cases.display.issues;

import haxe.display.JsonModuleTypes.JsonClassFieldScope;

class Issue7069 extends DisplayTestCase {
	/**
		class Main {
			static var field:Int;
			static function main(argument:Int) {
				var local:Int;
				{
					var blockLocal:Int;
					for (i in 0...10) {
						{-1-}
					}
				}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var items = result.items;
		function indexOfLocal(name:String) {
			for (i in 0...items.length) {
				switch items[i].kind {
					case Local if (items[i].args.name == name): return i;
					case ClassField if (items[i].args.field.name == name && (items[i].args.field.scope : Dynamic) == (JsonClassFieldScope.Static : Dynamic)): return i;
					case _:
				}
			}
			return -1;
		}
		var iIdx = indexOfLocal("i");
		var blockLocalIdx = indexOfLocal("blockLocal");
		var localIdx = indexOfLocal("local");
		var argumentIdx = indexOfLocal("argument");
		var fieldIdx = indexOfLocal("field");
		Assert.isTrue(iIdx != -1);
		Assert.isTrue(blockLocalIdx != -1);
		Assert.isTrue(localIdx != -1);
		Assert.isTrue(argumentIdx != -1);
		Assert.isTrue(fieldIdx != -1);
		Assert.isTrue(iIdx < blockLocalIdx);
		Assert.isTrue(blockLocalIdx < localIdx);
		Assert.isTrue(localIdx < argumentIdx);
		Assert.isTrue(argumentIdx < fieldIdx);
	}
}
