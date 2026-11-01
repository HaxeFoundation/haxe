package cases.display.issues;

import haxe.display.JsonModuleTypes;

class CompletionOriginMinimal extends DisplayTestCase {
	/**
		class Base {
			public var a:Int;
			public var b:String;
			public var c:Float;
			public function new() {}
			public function foo() {}
			public function bar() {}
		}

		class Child extends Base {
			public function new() super();
		}

		class Main {
			static function main() {
				var child = new Child();
				child.{-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		// A field inherited from Base carries an origin pointing at Base. That origin
		// must only identify the declaring type, not drag Base's whole field list into
		// the item (the bloat fix). Assert the origin's class args carry no fields/statics.
		var sawInherited = false;
		for (item in result.items) {
			switch item.kind {
				case ClassField:
					var occurrence:{origin:ClassFieldOrigin<Dynamic>} = cast item.args;
					var origin = occurrence.origin;
					if (origin == null || origin.args == null) continue;
					switch origin.kind {
						case Parent:
							sawInherited = true;
							var mt:JsonModuleType<Dynamic> = origin.args;
							// path/name must still be present so the client can render "from Base"
							Assert.equals("Base", mt.name);
							switch mt.kind {
								case Class:
									var cl:JsonClass = mt.args;
									Assert.equals(0, cl.fields.length);
									Assert.equals(0, cl.statics.length);
									Assert.isNull(cl.constructor);
								case _:
									Assert.fail("expected Class origin");
							}
						case _:
					}
				case _:
			}
		}
		Assert.isTrue(sawInherited);
	}
}
