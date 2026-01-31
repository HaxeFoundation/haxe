package cases;

import haxe.display.JsonModuleTypes;

using Lambda;

class Issue11484 extends DisplayTestCase {
	/**
		class Foo {}

		class Main {
			static function main() {
				{-1-}new Foo(1, "test"){-2-};
			}
		}
	**/
	function test() {
		var d = diagnostics();
		var range = diagnosticsRange(pos(1), pos(2));
		final item = d.find(d -> d.kind == MissingFields && utest.Assert.same(d.range, range));
		assert(item != null);
		final args:MissingFieldDiagnostics = item.args;
		final field:MissingField = args.entries[0].fields[0];
		eq("new", field.field.name);
		final jsonType:JsonType<JsonFunctionSignature> = field.field.type;
		final args = jsonType.args.args;
		eq("Int", args[0].t.args.path.typeName);
		eq("String", args[1].t.args.path.typeName);
		eq("Void", jsonType.args.ret.args.path.typeName);
	}
}
