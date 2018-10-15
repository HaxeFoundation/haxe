import haxe.Serializer.run;

class RunSerializerOutput {
	static function main() {
		new RunSerializerOutput();
	}

	var object:{};

	function new() {
		object = {};
		add("nul", null);
		add("int0", 0);
		add("intMinus1", -1);
		add("int1", 1);
		add("floatNaN", Math.NaN);
		add("floatPosInf", Math.POSITIVE_INFINITY);
		add("floatNegInf", Math.NEGATIVE_INFINITY);
		add("boolTrue", true);
		add("boolFalse", false);
		add("arrayEmpty", []);
		add("array0", [0]);
		add("arrayNull", [null]);
		add("array01", [0, 1]);

		var smap = new Map<String, String>();
		add("stringMapEmpty", smap);
		smap["foo"] = null;
		add("stringMapFooNull", smap);
		smap["foo"] = "bar";
		add("stringMapFooBar", smap);

		var imap = new Map<Int, String>();
		add("intMapEmpty", imap);
		imap[0] = "1";
		add("intMap01", imap);
		imap[1] = "2";
		add("intMap0112", imap);

		var omap = new Map<{}, String>();
		add("objectMapEmpty", omap);
		// TODO

		var s = haxe.Json.stringify(object);
		sys.io.File.saveContent("../serializedValues.txt", s);
	}

	function add<T>(key:String, value:T) {
		Reflect.setField(object, key, run(value));
	}
}