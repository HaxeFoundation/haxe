package unit.teststd.js.lib;

class TestBigInt extends Test {
	#if js
	function testBigInt() {
		var big = new js.lib.BigInt("9007199254740993");
		eq("9007199254740993", big.toString());
		eq("ff", new js.lib.BigInt(255).toString(16));
		eq("-1", js.lib.BigInt.asIntN(8, new js.lib.BigInt(255)).toString());
		eq("255", js.lib.BigInt.asUintN(8, new js.lib.BigInt(-1)).toString());
	}

	function testBigIntArrays() {
		eq(8, js.lib.BigInt64Array.BYTES_PER_ELEMENT);
		var signed = new js.lib.BigInt64Array(2);
		signed[0] = new js.lib.BigInt(-5);
		eq("-5", signed[0].toString());

		var unsigned = new js.lib.BigUint64Array(signed.buffer);
		eq("18446744073709551611", unsigned[0].toString());
	}

	function testFloat16Array() {
		// Only recent engines (e.g. Node.js 24+) provide Float16Array
		if (js.Syntax.typeof(js.Syntax.code("globalThis.Float16Array")) == "undefined") {
			noAssert();
			return;
		}

		eq(2, js.lib.Float16Array.BYTES_PER_ELEMENT);
		var a = new js.lib.Float16Array([1.5, 70000.0]);
		eq(1.5, a[0]);
		eq(Math.POSITIVE_INFINITY, a[1]);
		eq(1.5, a.find(x -> x > 1));
	}
	#end
}
