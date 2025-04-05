package unit;

class TestModuleStatics extends Test {
	function testVars() {
		eq("finalInit", finalInit);
		eq("finalHintInit", finalHintInit);
		eq("inlineFinalInit", inlineFinalInit);
		eq("inlineFinalHintInit", inlineFinalHintInit);
		eq("privateFinalInit", privateFinalInit);
		eq("privateFinalHintInit", privateFinalHintInit);
		eq("privateInlineFinalInit", privateInlineFinalInit);
		eq("privateInlineFinalHintInit", privateInlineFinalHintInit);
		eq("inlinePrivateFinalInit", inlinePrivateFinalInit);
		eq("inlinePrivateFinalHintInit", inlinePrivateFinalHintInit);
		eq("varInit", varInit);
		eq("varInitHint", varInitHint);
		eq(null, varHint);
		eq("inlineVarInit", inlineVarInit);
		eq("inlineVarInitHint", inlineVarInitHint);
		eq("privateVarInit", privateVarInit);
		eq("privateVarInitHint", privateVarInitHint);
		eq(null, privateVarHint);
		eq("privateInlineVarInit", privateInlineVarInit);
		eq("privateInlineVarInitHint", privateInlineVarInitHint);
		eq("inlinePrivateVarInit", inlinePrivateVarInit);
		eq("inlinePrivateVarInitHint", inlinePrivateVarInitHint);


		varInit = "1";
		eq("1", varInit);
		varInitHint = "2";
		eq("2", varInitHint);
		varHint = "3";
		eq("3", varHint);
		privateVarInit = "4";
		eq("4", privateVarInit);
		privateVarInitHint = "5";
		eq("5", privateVarInitHint);
		privateVarHint = "6";
		eq("6", privateVarHint);
	}

	function testFuncs() {
		eq("func", func());
		eq("privateFunc", privateFunc());
		eq("privateInlineFunc", privateInlineFunc());
		eq("inlinePrivateFunc", inlinePrivateFunc());
		eq("dynamicFunc", dynamicFunc());
		eq("privateDynamicFunc", privateDynamicFunc());
		eq("dynamicPrivateFunc", dynamicPrivateFunc());

		dynamicFunc = () -> "1";
		eq("1", dynamicFunc());
		privateDynamicFunc = () -> "2";
		eq("2", privateDynamicFunc());
		dynamicPrivateFunc = () -> "3";
		eq("3", dynamicPrivateFunc());
	}

	function testProps() {
		eq("prop-get", prop);
		prop = "hello";
		eq("hello-set-get", prop);
	}

	function testMacroDefined() {
		eq(42, mstatics.Funcs.funcA());
		eq(43, mstatics.Funcs.funcB());
		eq(44, mstatics.FuncC.FuncC());
	}
}

// macro-define the functions
typedef T = TestModuleStaticsMacro<Void>;

final finalInit = "finalInit";
final finalHintInit:String = "finalHintInit";
inline final inlineFinalInit = "inlineFinalInit";
inline final inlineFinalHintInit:String = "inlineFinalHintInit";
private final privateFinalInit = "privateFinalInit";
private final privateFinalHintInit:String = "privateFinalHintInit";
private inline final privateInlineFinalInit = "privateInlineFinalInit";
private inline final privateInlineFinalHintInit:String = "privateInlineFinalHintInit";
inline private final inlinePrivateFinalInit = "inlinePrivateFinalInit";
inline private final inlinePrivateFinalHintInit:String = "inlinePrivateFinalHintInit";
var varInit = "varInit";
var varInitHint:String = "varInitHint";
var varHint:String;
inline var inlineVarInit = "inlineVarInit";
inline var inlineVarInitHint:String = "inlineVarInitHint";
private var privateVarInit = "privateVarInit";
private var privateVarInitHint:String = "privateVarInitHint";
private var privateVarHint:String;
private inline var privateInlineVarInit = "privateInlineVarInit";
private inline var privateInlineVarInitHint:String = "privateInlineVarInitHint";
inline private var inlinePrivateVarInit = "inlinePrivateVarInit";
inline private var inlinePrivateVarInitHint:String = "inlinePrivateVarInitHint";

function func() return "func";
private function privateFunc() return "privateFunc";
private inline function privateInlineFunc() return "privateInlineFunc";
inline private function inlinePrivateFunc() return "inlinePrivateFunc";
dynamic function dynamicFunc() return "dynamicFunc";
private dynamic function privateDynamicFunc() return "privateDynamicFunc";
dynamic private function dynamicPrivateFunc() return "dynamicPrivateFunc";

@:isVar var prop(get,set):String = "prop";
function get_prop() return prop + "-get";
function set_prop(value) return prop = value + "-set";
