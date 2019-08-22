package misc.issue7877;

import haxe.macro.Expr;
import haxe.macro.Context;

class ProcessMacro {
	public static macro function build():Array<Field> {
		var fields = Context.getBuildFields();
		var toInit = [
			for (field in fields) {
				switch (field) {
					case {name: name, kind: FVar(t, e), access: [AFinal]}:
						{name: name, type: t, def: e};
					case _:
						continue;
				}
			}
		];

		var args:Array<FunctionArg> = [];
		var exprs = [];
		for (init in toInit) {
			args.push({
				name: init.name,
				opt: init.def != null,
				type: init.type,
				value: init.def
			});
			var n = init.name;
			exprs.push(macro this.$n = $i{n});
		}
		fields.push({
			pos: Context.currentPos(),
			name: 'new',
			access: [APublic],
			kind: FFun({ret: null, args: args, expr: {pos: Context.currentPos(), expr: EBlock(exprs)}})
		});
		return fields;
	}
}
