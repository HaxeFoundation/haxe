import haxe.macro.Context;

function main() {
	// dash set then underscore
	if (Context.definedValue("value-a") != "new value")
		throw '`value-a` flag has incorrect value. Expected: `new value`, Got: `${Context.definedValue("value-a")}`';
	if (Context.definedValue("value_a") != "new value")
		throw '`value_a` flag has incorrect value. Expected: `new value`, Got: `${Context.definedValue("value_a")}`';

	if (Context.definedValue("value-b") != "new value")
		throw '`value-b` flag has incorrect value. Expected: `new value`, Got: `${Context.definedValue("value-b")}`';
	if (Context.definedValue("value_b") != "new value")
		throw '`value_b` flag has incorrect value. Expected: `new value`, Got: `${Context.definedValue("value_b")}`';

	// underscore set then dash
	if (Context.definedValue("value-c") != "new value")
		throw '`value-c` flag has incorrect value. Expected: `new value`, Got: `${Context.definedValue("value-c")}`';
	if (Context.definedValue("value_c") != "new value")
		throw '`value_c` flag has incorrect value. Expected: `new value`, Got: `${Context.definedValue("value_c")}`';

	if (Context.definedValue("value-d") != "new value")
		throw '`value-c` flag has incorrect value. Expected: `new value`, Got: `${Context.definedValue("value-d")}`';
	if (Context.definedValue("value_d") != "new value")
		throw '`value_c` flag has incorrect value. Expected: `new value`, Got: `${Context.definedValue("value_d")}`';
}
