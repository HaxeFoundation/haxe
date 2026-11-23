import haxe.macro.Context;
import haxe.macro.Type;

class Mac {
	static function init() {
		Context.onGenerate(function(types) {
			for (t in types) switch t {
				case TEnum(e, _) if (e.get().name == "TheirEnum" || e.get().name == "MyEnum"):
					var e = e.get();
					var n = 0;
					for (c in e.constructs) for (m in c.meta.get()) if (m.name == ":realPath") n++;
					Sys.println('realPath ${e.name} $n');
				case _:
			}
		});
	}
}
