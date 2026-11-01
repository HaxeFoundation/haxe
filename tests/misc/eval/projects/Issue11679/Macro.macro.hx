import haxe.macro.Context;

function build() {
	switch (Context.getLocalClass().get().name) {
		case "Foo":
			switch Context.getType("Bar") {
				case TInst(cls,_): cls.get();
				case _:
			}
		case "Bar":
			throw "Ohno";
	}
	return null;
}
