package unit.issues;

import haxe.io.Error;

private enum FirstErrorEnum {
	ERROR1;
}

private enum SecondErrorEnum {
	ERROR2;
}

class Issue12151 extends unit.Test {
	function test() {
		var e:Dynamic = Error.Custom("EOF");
		f(Std.isOfType(e, FirstErrorEnum));
		f(Std.isOfType(e, SecondErrorEnum));
		t(Std.isOfType(e, Error));

		eq("Error", catchIt(Error.Custom("EOF")));
		eq("FirstErrorEnum", catchIt(ERROR1));
		eq("SecondErrorEnum", catchIt(ERROR2));
	}

	static function catchIt(value:Dynamic) {
		try {
			throw value;
		} catch (e:FirstErrorEnum) {
			return "FirstErrorEnum";
		} catch (e:Error) {
			return "Error";
		} catch (e:SecondErrorEnum) {
			return "SecondErrorEnum";
		}
	}
}
