enum FlatEnum {
	A;
	B;
}

enum ThickEnum {
	C;
	DD(i:Int);
}

function main() {
	var flat1 = A;
	var flat2 = B;
	if (flat1 == flat2) {}
	if (flat1 != flat2) {}

	var thick1 = C;
	var thick2 = DD(1);
	if (thick1 == thick2) {}
	if (thick1 != thick2) {}
}