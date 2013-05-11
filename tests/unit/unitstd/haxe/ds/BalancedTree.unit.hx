var test = [
	13 => 1,
	8 => 2,
	17 => 3,
	1 => 4,
	11 => 5,
	15 => 6,
	25 => 7,
	6 => 8,
	22 => 9,
	27 => 10
];
var otherKeys = [for (i in 0...30) if (!test.exists(i)) i];
var m = new haxe.ds.BalancedTree<Int, Int>();
for (k in test.keys()) {
	m.set(k, test[k]);
}
for (k in test.keys()) {
	eq(test[k], m.get(k));
}
for (k in test.keys()) {
	eq(true, m.exists(k));
}
for (k in otherKeys) {
	eq(false, m.exists(k));
}
for (k in test.keys()) {
	eq(true, m.remove(k));
	eq(false, m.exists(k));
}