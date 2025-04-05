#if macro
import haxe.macro.Context;
import haxe.macro.Expr;

typedef SafetyMessage = {msg:String, pos:Position}
typedef ExpectedMessage = {symbol:String, pos:Position}
#end

class Validator {
#if macro
	static var expectedErrors:Array<ExpectedMessage> = [];

	static dynamic function onNullSafetyReport(callback:(errors:Array<SafetyMessage>)->Void):Void {
	}

	static public function register() {
		expectedErrors = [];
		onNullSafetyReport = @:privateAccess Context.load("on_null_safety_report", 1);
		onNullSafetyReport(validate);
	}

	static public function checkFields():Array<Field> {
		for(field in Context.getBuildFields()) {
			for(meta in field.meta) {
				if(meta.name == ':shouldFail') {
					var fieldPosInfos = Context.getPosInfos(field.pos);
					fieldPosInfos.min = Context.getPosInfos(meta.pos).max + 1;
					expectedErrors.push({symbol: field.name, pos:Context.makePosition(fieldPosInfos)});
					break;
				}
			}
		}
		return null;
	}

	static function validate(errors:Array<SafetyMessage>) {
		var errors = check(expectedErrors.copy(), errors.copy());
		if(errors.ok) {
			Sys.println('${errors.passed} expected errors spotted');
			Sys.println('Compile-time tests passed.');
		} else {
			Context.error('Tests failed with ${errors.failed} failures. See warnings.', Context.currentPos());
		}
	}

	static function check(expected:Array<ExpectedMessage>, actual:Array<SafetyMessage>):{ok:Bool, passed:Int, failed:Int} {
		var passed = 0;
		var i = 0;
		while(i < actual.length) {
			var actualEvent = actual[i];
			var wasExpected = false;
			for(expectedEvent in expected) {
				if(posContains(expectedEvent.pos, actualEvent.pos)) {
					expected.remove(expectedEvent);
					wasExpected = true;
					break;
				}
			}
			if(wasExpected) {
				actual.remove(actualEvent);
				++passed;
			} else {
				++i;
			}
		}
		actual.reverse();
		for(event in actual) {
			Context.warning(event.msg, event.pos);
		}
		for(event in expected) {
			Context.warning('${event.symbol} was expected to fail, but it did not fail.', event.pos);
		}
		return {
			ok: actual.length == 0 && expected.length == 0,
			passed: passed,
			failed: actual.length + expected.length
		}
	}

	static function posContains(pos:Position, subPos:Position):Bool {
		var infos = Context.getPosInfos(pos);
		var subInfos = Context.getPosInfos(subPos);
		return infos.file == subInfos.file && infos.min <= subInfos.min && infos.max >= subInfos.max;
	}
#end

	macro static public function shouldFail(expr:Expr):Expr {
		expectedErrors.push({symbol:Context.getLocalMethod(), pos:expr.pos});
		return expr;
	}
}