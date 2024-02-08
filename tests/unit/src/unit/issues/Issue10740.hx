package unit.issues;

import unit.HelperMacros.typeString;

class Issue10740 extends Test {
	// not local vars to prevent optimizations
	var o:Null<S> = null;
	var o2:Null<S & {sideEffectCount:() -> Int}> = {
		var sideEffectCount = 0;
		{
			field1: 42,
			field2: "hallo",
			field3: {field4: true},
			field5: function() {
				sideEffectCount++;
				return [{field6: 28}];
			},
			sideEffectCount: () -> sideEffectCount
		};
	};

	function testTyping() {
		// simple field access
		eq("Null<Int>", typeString(o?.field1));
		eq("Null<String>", typeString(o?.field2));

		// field chain
		eq("Null<Bool>", typeString(o?.field3.field4));

		// complex chain
		eq("Null<Int>", typeString(o?.field5()[0].field6));

		// nested chain
		eq("Null<Int>", typeString(o?.field5()[0]?.field6));
	}

	function testNull() {
		// simple field access
		eq(null, o?.field1);
		eq(null, o?.field2);

		// field chain
		eq(null, o?.field3.field4);

		// complex chain
		eq(null, o?.field5()[0].field6);

		// nested chain
		eq(null, o?.field5()[0]?.field6);
	}

	function testNonNull() {
		// simple field access
		eq(42, o2?.field1);
		eq("hallo", o2?.field2);

		// field chain
		eq(true, o2?.field3.field4);

		// complex chain
		eq(28, o2?.field5()[0].field6);
		eq(1, o2.sideEffectCount());

		// nested chain
		eq(28, o2?.field5()[0]?.field6);
		eq(2, o2.sideEffectCount());
		eq(null, o2?.field5()[1]?.field6);
		eq(3, o2.sideEffectCount());
	}
}

private typedef S = {
	var field1:Int;
	var field2:String;
	var field3:{field4:Bool};
	var field5:() -> Array<{field6:Null<Int>}>;
}
