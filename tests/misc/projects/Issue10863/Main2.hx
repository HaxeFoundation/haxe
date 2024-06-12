#if macro
typedef TypeForMacroContext = {}
#else
typedef TypeForNormalContext = {}
#end

function main() {
	#if macro
	var a:TypeForMacroContext = {};
	#else
	var a:TypeForNormalContext = {};
	#end
	$type(a);
	foo();
}

macro function foo() return macro {}
