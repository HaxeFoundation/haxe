var arrEq = function(arrA:Array<Dynamic>, arrB:Array<Dynamic>, ?pos : haxe.PosInfos) {
	eq(arrA.length, arrB.length, pos);
	for (i in 0...arrA.length) {
		eq(arrA[i], arrB[i], pos);
	}
};

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

var copied = m.copy();
copied != m;
for(k in m.keys()) {
	eq(test[k], copied.get(k));
	copied.set(k, copied.get(k) + 1);
	eq(test[k] + 1, copied.get(k));
	eq(test[k], m.get(k));
}

var r = [for (key in m.keys()) key];
arrEq(r, [1,6,8,11,13,15,17,22,25,27]);
var r = [for (val in m) val];
arrEq(r, [4,8,2,5,1,6,3,9,7,10]);
for (k in test.keys()) {
	eq(true, m.remove(k));
	eq(false, m.exists(k));
}

var ms = new haxe.ds.BalancedTree<String, Int>();
for (k in test.keys()) {
	ms.set(Std.string(k), test[k]);
}
for (k in test.keys()) {
	eq(ms.get(Std.string(k)), test[k]);
}
for (k in test.keys()) {
	eq(ms.exists(Std.string(k)), true);
}
for (k in otherKeys) {
	eq(ms.exists(Std.string(k)), false);
}
var r2 = [for (key in ms.keys()) key];
arrEq(r2, [for (k in [1,11,13,15,17,22,25,27,6,8]) Std.string(k)]);

var r = [for (val in ms) val];
arrEq(r, [4,5,1,6,3,9,7,10,8,2]);
for (k in test.keys()) {
	eq(ms.remove(Std.string(k)), true);
	eq(ms.exists(Std.string(k)), false);
}
