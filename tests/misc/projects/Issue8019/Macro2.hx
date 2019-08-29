import haxe.macro.Context;

class Macro2 {
	static function init() {
		function defineModule(name:String) {
			Context.defineModule(name, [{
				pos: (macro 0).pos,
				pack: [],
				name: name,
				kind: TDClass(),
				fields: []
			}]);
		}

		defineModule("Valid");

		defineModule("");
		defineModule("0");
		defineModule("Type+");
	}
}
