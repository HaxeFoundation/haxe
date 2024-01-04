import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
import haxe.macro.TypeTools;

class Macro {
	public static function logBuild() {
		Sys.println('Building ${Context.getLocalClass().toString()}.');
		return null;
	}

	@:persistent static var generated = new Map<String, Bool>();

	static function isAlive(ct:ComplexType, pos:Position):Bool {
		// Null check is just there to make it a one liner
		// Basically returning true if no exception is caught
		return try Context.resolveType(ct, pos) != null catch(e) false;
	}

	public static function buildFoo() {
		var from = '[${Context.getLocalModule()}] ';
		var print = s -> Sys.println(from + s);

		switch (Context.getLocalType()) {
			case TInst(_, [target]):
				var pos = Context.currentPos();
				var bt = TypeTools.toBaseType(target);
				var key = ["Foo", bt.module, bt.name].join("__");
				var ct = TPath({pack: [], name: key});

				if (generated.exists(key)) {
					if (isAlive(ct, pos)) {
						print('Reusing previously generated type for $key.');
						return ct;
					}

					print('Previously generated type for $key has been discarded.');
				}

				var genDef = macro class $key {
					static function __init__() Sys.println("[runtime] Hello from " + $v{key});
				};

				// Not really needed but nicer
				// genDef.pos = pos;

				// Not needed unless dce full
				// genDef.meta.push({name: ":keep", params: [], pos: pos});

				print('Generating type for $key.');
				#if config.defineModule
				Context.defineModule(key, [genDef]);
				#else
				Context.defineType(genDef, bt.module);
				#end

				generated.set(key, true);
				return ct;

			case _: throw "";
		}
	}
}
