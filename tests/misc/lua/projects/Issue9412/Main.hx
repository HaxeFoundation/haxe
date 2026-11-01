function keep(a:Int) untyped {}

function test(a:Int, b:Int) {
	keep(a & b);
	keep(a | b);
	keep(a ^ b);
}

function main() {
	test(0xF0F0, 0x0FF0);
}
