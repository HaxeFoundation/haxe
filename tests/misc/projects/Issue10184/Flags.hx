import haxe.macro.Context;

function main() {
	// set with dash
	if (!Context.defined("f-dash"))
		throw "`f-dash` flag is missing";
	if (!Context.defined("f_dash"))
		throw "`f_dash` flag is missing";

	// set with underscore
	if (!Context.defined("f-underscore"))
		throw "`f-underscore` flag is missing";
	if (!Context.defined("f_underscore"))
		throw "`f_underscore` flag is missing";

	// value set with dash
	if (Context.definedValue("v-dash") != "value")
		throw "`v-dash` flag has incorrect value: " + Context.definedValue("v-dash");
	if (Context.definedValue("v_dash") != "value")
		throw "`v_dash` flag has incorrect value" + Context.definedValue("v_dash");

	// value set with underscore
	if (Context.definedValue("v-underscore") != "value")
		throw "`v-underscore` flag has incorrect value" + Context.definedValue("v-underscore");
	if (Context.definedValue("v_underscore") != "value")
		throw "`v_underscore` flag has incorrect value" + Context.definedValue("v-underscore");
}
