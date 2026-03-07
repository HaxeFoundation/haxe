#if !macro
function main() testMacro();
#end

macro function testMacro() {
	var p1 = (macro "1st position").pos;
	var p2 = (macro "2nd position").pos;
	haxe.macro.Context.error("Top level error", haxe.macro.Context.currentPos(), [
		{msg: "1st sub error", pos: p1, sub: [{msg: "Nested sub error", pos: p2}]},
		{msg: "2nd sub error", pos: p2}
	]);
	return macro null;
}
