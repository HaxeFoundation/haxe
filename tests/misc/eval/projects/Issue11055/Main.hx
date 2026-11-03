macro function test() {
	foo();
	return macro null;
}

#if macro
function foo() {
	throw "up";
}
#end

function main() {
	test();
}
