function main() {
	final defines = haxe.macro.Context.getDefines();

	// -D f-dash
	if (!defines.exists("f-dash"))
		throw "`f-dash` flag is missing";
	if (!defines.exists("f_dash"))
		throw "`f_dash` flag is missing";

	// -D f_underscore
	if (!defines.exists("f-underscore"))
		throw "`f-underscore` flag is missing";
	if (!defines.exists("f_underscore"))
		throw "`f_underscore` flag is missing";

	// -D v-dash=value
	if (defines["v-dash"] != "value")
		throw "`v-dash` flag value is incorrect: " + defines["v-dash"];
	if (defines["v_dash"] != "value")
		throw "`v_dash` flag value is incorrect: " + defines["v_dash"];

	// -D v_underscore=value
	if (defines["v-underscore"] != "value")
		throw "`v-underscore` flag value is incorrect: " + defines["v-underscore"];
	if (defines["v_underscore"] != "value")
		throw "`v_underscore` flag value is incorrect: " + defines["v_underscore"];
}
