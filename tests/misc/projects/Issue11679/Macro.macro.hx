import haxe.macro.Context;

	function build() {
		switch (Context.getLocalClass().get().name) {
			case "Foo":
				switch Context.getType("Bar") {
					case TInst(cls,_): cls.get();
					case _:
				}
			case "Bar":
				Context.parseInlineString('& if (!r.can) {}', Context.currentPos());
		}
		return null;
	}
