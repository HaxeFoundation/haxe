function throwInactive(flag:String) {
	throw '`$flag` block is not active';
}

function throwInactiveValue(flag:String, expected:String, value:String) {
	throw '`$flag` block is not active, as flag has value `$value` instead of `$expected`';
}

function main() {
	#if !f_dash
	throwInactive("f_dash");
	#end

	#if !f_underscore
	throwInactive("f_underscore");
	#end

	#if (v_dash!="value")
	throwInactiveValue("v_dash", "value", haxe.macro.Context.definedValue("v_dash"));
	#end

	#if (v_underscore!="value")
	throwInactiveValue("v_underscore", "value", haxe.macro.Context.definedValue("v_underscore"));
	#end
}
