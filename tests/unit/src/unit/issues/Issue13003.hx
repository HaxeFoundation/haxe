package unit.issues;

class Issue13003 extends Test {
	#if js
	function test() {
		eq("Array<Int>", bytesToHexLookupType());

		var bytes = haxe.io.Bytes.alloc(6);
		for (index => value in [0x00, 0x0F, 0x10, 0x7F, 0x80, 0xFF])
			bytes.set(index, value);
		eq("000f107f80ff", bytes.toHex());
	}
	#end

	macro static function bytesToHexLookupType() {
		var bytes = switch haxe.macro.Context.getType("haxe.io.Bytes") {
			case TInst(type, _):
				type.get();
			case type:
				haxe.macro.Context.error('Expected haxe.io.Bytes to be a class, got ${haxe.macro.TypeTools.toString(type)}', haxe.macro.Context.currentPos());
		}
		var toHex = Lambda.find(bytes.fields.get(), field -> field.name == "toHex");
		if (toHex == null)
			haxe.macro.Context.error("Could not find haxe.io.Bytes.toHex", bytes.pos);

		var lookupType = null;
		function visit(expression:haxe.macro.Type.TypedExpr) {
			switch expression.expr {
				case TVar(variable, _) if (variable.name == "chars"):
					lookupType = haxe.macro.TypeTools.toString(variable.t);
				case _:
					haxe.macro.TypedExprTools.iter(expression, visit);
			}
		}
		visit(toHex.expr());
		if (lookupType == null)
			haxe.macro.Context.error("Could not find the haxe.io.Bytes.toHex lookup table", toHex.pos);
		return macro $v{lookupType};
	}
}
