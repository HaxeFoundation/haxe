package;

import haxe.ds.Option;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

class Macro {
	public static function build() {
		switch Context.getLocalType() {
			case TInst(_, [Context.follow(_) => TAnonymous(_.get() => a)]):
				return buildAnon(a);
			case _: throw 'assert';
		}
	}
	
	
	static function buildAnon(a:AnonType):Type {
		var sig = Context.signature(a);
		var name = "Type" + sig;
		try return Context.getType(name) catch(e:Dynamic) {}
		var pos = Context.currentPos();
		var fields = [];
		for(field in a.fields) {
			var meta = field.meta.get();
			
			fields.push({
				kind: FieldType.FVar(Context.toComplexType(field.type)),
				name: field.name,
				pos: field.pos,
				meta: meta,
			});
		}
		
		Context.defineType({
			fields: fields,
			kind: TDStructure,
			name: name,
			pack: [],
			pos: pos,
		});
		
		return Context.getType(name);
	}
}