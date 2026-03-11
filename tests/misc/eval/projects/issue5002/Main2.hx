import haxe.macro.Context;
import haxe.macro.Expr;

class Main2 {
	static function main() {
		invalidVariable("0_variable");
		invalidVariable("var");
		invalidVariable("new");
		invalidVariable("foo \"\t\n");
		invalidCatchVariable();
		invalidFunctionName();
		invalidFunctionArgumentName();
		invalidPatternVariable();
		invalidForVariable();
		invalidForVariableKeyValue();
	}

	static macro function invalidVariable(name:String) {
		return {
			expr: EVars([{name: name, type: null, expr: null}]),
			pos: Context.currentPos()
		};
	}

	static macro function invalidCatchVariable() {
		return {
			expr: ETry(macro {}, [{name: "0_catchVariable", type: macro:Dynamic, expr: macro {}}]),
			pos: Context.currentPos()
		};
	}

	static macro function invalidFunctionName() {
		return {
			expr: EFunction(FNamed("0_function"), {
				args: [],
				ret: macro:Void,
				expr: macro {}
			}),
			pos: Context.currentPos()
		};
	}

	static macro function invalidFunctionArgumentName() {
		return {
			expr: EFunction(FNamed("foo"), {
				args: [
					{
						name: "0_argument",
						type: macro:Int
					}
				],
				ret: macro:Void,
				expr: macro {}
			}),
			pos: Context.currentPos()
		};
	}

	static macro function invalidPatternVariable() {
		return {
			expr: ESwitch(macro "", [
				{
					values: [
						{
							expr: EConst(CIdent("0_patternVariable")),
							pos: Context.currentPos()
						}
					],
					expr: macro {}
				}
			], null),
			pos: Context.currentPos()
		};
	}

	static macro function invalidForVariable() {
		return {
			expr: EFor({
				expr: EBinop(OpIn, {
					expr: EConst(CIdent("0_forVariable")),
					pos: Context.currentPos()
				}, macro []),
				pos: Context.currentPos()
			}, macro {}),
			pos: Context.currentPos()
		};
	}

	static macro function invalidForVariableKeyValue() {
		return {
			expr: EFor({
				expr: EBinop(OpIn, {
					expr: EBinop(OpArrow, {
						expr: EConst(CIdent("0_forVariableKey")),
						pos: Context.currentPos()
					}, {
						expr: EConst(CIdent("0_forVariableValue")),
						pos: Context.currentPos()
					}),
					pos: Context.currentPos()
				}, macro new Map()),
				pos: Context.currentPos()
			}, macro {}),
			pos: Context.currentPos()
		};
	}
}
