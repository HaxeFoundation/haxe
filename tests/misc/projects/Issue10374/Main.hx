#if macro
import haxe.macro.Context;
#end

class Main {
	static function main() {
		test();
	}

	static macro function test() {
		if(try {Context.getType('Test.Sub'); false;} catch(_:Any) true) {
			Context.defineModule("Test", [{
				pos: Context.currentPos(),
				pack: [],
				name: 'Sub',
				params: [],
				meta: [],
				kind: TDAlias(macro :String),
				fields: []
			}, {
				pos: (macro moduleField).pos,
				pack: [],
				name: 'moduleField',
				params: [],
				meta: [],
				kind: TDField(FVar(null, macro null), [AInline, AFinal]),
				fields: []
			}]);
		}

		return macro {};
	}
}
