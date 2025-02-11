import haxe.macro.Context;

function defineType() {
	Context.onAfterInitMacros(() -> {
		Context.defineType({
			pos: Context.currentPos(),
			pack: [],
			name: "Foo",
			kind: TDAbstract(macro :String, [], [], []),
			fields: []
		});
	});
}

function defineModule() {
	Context.onAfterInitMacros(() -> {
		Context.defineModule("Bar", [{
			pos: Context.currentPos(),
			pack: [],
			name: "Bar",
			kind: TDAbstract(macro :String, [], [], []),
			fields: []
		}]);
	});
}
