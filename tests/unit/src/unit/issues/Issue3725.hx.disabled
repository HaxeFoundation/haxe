package unit.issues;

@:rtti
class Issue3725 extends Test {

	static function myStaticArgs(stringValue = "foo", intValue = 12, intHexValue = 0xFFFFFF, floatValue = 12.2223, boolValue = true) { }

	function testStaticArgs() {
		var rtti = haxe.rtti.Rtti.getRtti(Issue3725);
		var valueMap = new Map();
		for (cf in rtti.statics) {
			if (cf.name == "myStaticArgs") {
				switch (cf.type) {
					case CFunction(args, _):
						for (arg in args) {
							valueMap[arg.name] = arg.value;
						}
					case _:
				}
			}
		}
		eq('"foo"', valueMap["stringValue"]);
		eq("12", valueMap["intValue"]);
		eq("0xFFFFFF", valueMap["intHexValue"]);
		eq("12.2223", valueMap["floatValue"]);
		eq("true", valueMap["boolValue"]);
	}

	static var stringValue = "foo";
	static var intValue = 12;
	static var intHexValue = 0xFFFFFF;
	static var floatValue = 12.2223;
	static var boolValue = true;

	function testStaticFields() {
		var rtti = haxe.rtti.Rtti.getRtti(Issue3725);
		var valueMap = new Map();
		for (cf in rtti.statics) {
			valueMap[cf.name] = cf.expr;
		}
		eq('"foo"', valueMap["stringValue"]);
		eq("12", valueMap["intValue"]);
		eq("0xFFFFFF", valueMap["intHexValue"]);
		eq("12.2223", valueMap["floatValue"]);
		eq("true", valueMap["boolValue"]);
	}

	function myMemberArgs(stringValue = "foo", intValue = 12, intHexValue = 0xFFFFFF, floatValue = 12.2223, boolValue = true) { }

	function testMemberArgs() {
		var rtti = haxe.rtti.Rtti.getRtti(Issue3725);
		var valueMap = new Map();
		for (cf in rtti.fields) {
			if (cf.name == "myMemberArgs") {
				switch (cf.type) {
					case CFunction(args, _):
						for (arg in args) {
							valueMap[arg.name] = arg.value;
						}
					case _:
				}
			}
		}
		eq('"foo"', valueMap["stringValue"]);
		eq("12", valueMap["intValue"]);
		eq("0xFFFFFF", valueMap["intHexValue"]);
		eq("12.2223", valueMap["floatValue"]);
		eq("true", valueMap["boolValue"]);
	}

	var stringValueM = "foo";
	var intValueM = 12;
	var intHexValueM = 0xFFFFFF;
	var floatValueM = 12.2223;
	var boolValueM = true;

	function testMemberFields() {
		var rtti = haxe.rtti.Rtti.getRtti(Issue3725);
		var valueMap = new Map();
		for (cf in rtti.fields) {
			valueMap[cf.name] = cf.expr;
		}
		eq('"foo"', valueMap["stringValueM"]);
		eq("12", valueMap["intValueM"]);
		eq("0xFFFFFF", valueMap["intHexValueM"]);
		eq("12.2223", valueMap["floatValueM"]);
		eq("true", valueMap["boolValueM"]);
	}
}