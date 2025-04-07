import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.TypeTools;

class Macro {
	@:persistent static var generated = new Map<String, Bool>();

	static function isAlive(ct:ComplexType, pos:Position):Bool {
		// Null check is just there to make it a one liner
		// Basically returning true if no exception is caught
		return try Context.resolveType(ct, pos) != null catch(e) false;
	}

	public static function buildState() {
		var constructor = [];
		var fields = Context.getBuildFields().map(f -> {
			return switch f.kind {
				case _ if (f.access.contains(AStatic)): f;

				case FVar(null, _): f;
				case FVar(ct, e):
					var fname = f.name;
					constructor.push(macro this.$fname = new Proxy<$ct>(null));

					{
						name: f.name,
						doc: f.doc,
						pos: f.pos,
						meta: f.meta,
						access: f.access,
						kind: FieldType.FVar(macro :Proxy<$ct>, e)
					};

				case FProp(_) | FFun(_): f;
			}
		});

		if (constructor.length > 0) {
			fields.push({
				name: "new",
				doc: null,
				pos: Context.currentPos(),
				meta: [],
				access: [APublic],
				kind: FFun({
					args: [],
					ret: null,
					params: null,
					expr: macro $b{constructor}
				})
			});
		}

		return fields;
	}

	public static function buildProxy() {
		switch (Context.getLocalType()) {
			case TInst(_, [target]):
				var pos = Context.currentPos();
				var targetCt = TypeTools.toComplexType(target);
				var bt = TypeTools.toBaseType(target);
				var key = ["Proxy", bt.module, bt.name].join("__");
				var ct = TPath({pack: [], name: key});
				if (generated.exists(key) && isAlive(ct, pos)) return ct;

				var genDef = macro class $key {
					var data:$targetCt;
					public function new(data:$targetCt) this.data = data;
					public inline function get():$targetCt return data;
				};

				Context.defineType(genDef);
				generated.set(key, true);
				return ct;

			case _: throw "";
		}
	}
}
