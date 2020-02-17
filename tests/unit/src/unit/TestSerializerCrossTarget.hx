package unit;
import haxe.Unserializer;
import haxe.Resource;
import haxe.Json;

class TestSerializerCrossTarget extends Test {
	function testCrossTarget() {
		var values = Resource.getString("serializedValues.txt");
		var json = Json.parse(values);
		for (fieldName in Reflect.fields(json)) {
			var valueString = Reflect.field(json, fieldName);
			var func = Reflect.field(this, fieldName);
			if (func == null) {
				assert("Missing field: " + fieldName);
			} else {
				Reflect.callMethod(this, func, [Unserializer.run(valueString)]);
			}
		}
	}

	function nul<T>(value:T) {
		eq(null, value);
	}

	function int0(value:Int) {
		eq(0, value);
	}

	function int1(value:Int) {
		eq(1, value);
	}

	function intMinus1(value:Int) {
		eq(-1, value);
	}

	function floatNaN(value:Float) {
		t(Math.isNaN(value));
	}

	function floatNegInf(value:Float) {
		f(Math.isFinite(value));
		t(value < 0);
	}

	function floatPosInf(value:Float) {
		f(Math.isFinite(value));
		t(value > 0);
	}

	function boolFalse(value:Bool) {
		f(value);
	}

	function boolTrue(value:Bool) {
		t(value);
	}

	function arrayEmpty<T>(value:Array<T>) {
		eq(0, value.length);
	}

	function arrayNull<T>(value:Array<T>) {
		eq(1, value.length);
		eq(null, value[0]);
	}

	function array0(value:Array<Int>) {
		eq(1, value.length);
		eq(0, value[0]);
	}

	function array01(value:Array<Int>) {
		eq(2, value.length);
		eq(0, value[0]);
		eq(1, value[1]);
	}

	function stringMapEmpty(value:Map<String, String>) {
		f(value.keys().hasNext());
	}

	function stringMapFooNull(value:Map<String, String>) {
		eq(null, value["foo"]);
	}

	function stringMapFooBar(value:Map<String, String>) {
		eq("bar", value["foo"]);
	}

	function intMapEmpty(value:Map<Int, String>) {
		f(value.keys().hasNext());
	}

	function intMap01(value:Map<Int, String>) {
		eq("1", value[0]);
	}

	function intMap0112(value:Map<Int, String>) {
		eq("1", value[0]);
		eq("2", value[1]);
	}

	function objectMapEmpty(value:Map<{}, String>) {
		var keys = value.keys();
		f(keys.hasNext());
	}
}