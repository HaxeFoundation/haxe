import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.TypeTools;

class Macro {
	public static function build() {
		var fields = Context.getBuildFields();
		var cl = Context.getLocalClass().get();
		var itemType = Context.getLocalType();
		var itemComplexType = TypeTools.toComplexType(itemType);

		var type:TypeDefinition = {
			pos: Context.currentPos(),
			name: cl.name + "Collection",
			pack: cl.pack,
			kind: TDClass({pack: ["col"], name: "Collection", params: [TPType(itemComplexType)]}),
			fields: [],
		}

		Context.defineType(type);
		return fields;
	}
}
