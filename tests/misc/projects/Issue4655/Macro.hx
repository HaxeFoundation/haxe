import haxe.macro.Expr;
import haxe.macro.Context;

class Macro {
	macro static public function getMetaAndDocInfo(e:Expr) {
		var map = {};
		switch (Context.follow(Context.typeof(e))) {
			case TAnonymous(an):
				for (field in an.get().fields) {
					var meta = field.meta.get().map(function(m) return m.name);
					Reflect.setField(map, field.name, {
						doc: field.doc,
						meta: meta.join(", ")
					});
				}
			case _:
		}
		return macro $v{map};
	}
}