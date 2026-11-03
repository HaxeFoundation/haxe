macro function test() {
	haxe.macro.Context.parseInlineSring("p:true,v:0", haxe.macro.Context.currentPos())
	return macro null;
}

function main() {
	test();
}