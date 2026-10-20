import haxe.macro.Context;

function build():Array<haxe.macro.Expr.Field> {
	switch (Context.getLocalClass().get().name) {
		case "Foo":
			switch Context.getType("Bar") {
				case TInst(cls,_): cls.get().fields.get();
				case _:
			}
		case "Bar":
			throw "Ohno";
	}
	return null;
}
