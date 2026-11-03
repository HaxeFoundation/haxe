function main() {
	var a = null;
	if (a == null) a = 10;
	$type(a); // Null<Null<Int>>

	var b = null;
	$type(b); // Null<Unknown<0>>
	b = null;
	$type(b); // Null<Null<Unknown<0>>>
	b = null;
	$type(b); // Null<Null<Null<Unknown<0>>>>
	b = 10;
	$type(b); // Null<Null<Null<Int>>>
}