package unit.issues;

// A schema for some JSON file
private typedef Format = {
    ?anchorX :Float,
    ?anchorY :Float,
};

// Returns value with a default if missing
/* inline */ function deNull<T> (value :T, defaultValue :T) :T {
    return (value != null) ? value : defaultValue;
}

class Issue9590 extends Test {
	function test() {
		// Load a bunch of JSON data
		var obj :Format = haxe.Json.parse("{\"anchorX\": 123.0}");

		var anchorX :Float = deNull(obj.anchorX, 0.0);
		var anchorY :Float = deNull(obj.anchorY, 0.0);

		feq(123.0, anchorX);
		feq(0, anchorY);
	}
}