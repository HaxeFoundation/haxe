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

// keyValueIterator
var test2 = new haxe.ds.BalancedTree<Int, Int>();
var keys1 = [1,2,3];
var values1 = [2,4,6];
for(i in 0 ... keys1.length) test2.set(keys1[i], values1[i]);

[for(k=>v in test2) k] == [1,2,3];
[for(k=>v in test2) v] == [2,4,6];
[for(k=>v in test2) k*v] == [2,8,18];

// clear
var test3 = new haxe.ds.BalancedTree<Int, Int>();
test3.set(0, 1);
test3.set(2, 3);
test3.set(4, 6);

[for(k=>v in test3) k] == [0,2,4];

test3.clear();

[for(k=>v in test3) k] == [];
test3.exists(0) == false;
test3.exists(2) == false;
test3.exists(4) == false;

test3.set(0, 1);
test3.exists(0) == true;
test3.exists(2) == false;
test3.exists(4) == false;



var entryEq = function(
	a:haxe.ds.BalancedTree.Entry<Dynamic,Dynamic>,
	b:haxe.ds.BalancedTree.Entry<Dynamic,Dynamic>,
	?pos : haxe.PosInfos
) {
	eq(a.key, b.key, pos);
	eq(a.value, b.value, pos);
};
var neighEq = function(
	a:haxe.ds.BalancedTree.Neighborhood<Dynamic,Dynamic>,
	b:haxe.ds.BalancedTree.Neighborhood<Dynamic,Dynamic>,
	?pos : haxe.PosInfos
) {
	entryEq(a.prev, b.prev, pos);
	entryEq(a.ident, b.ident, pos);
	entryEq(a.next, b.next, pos);
};
var nullEntry = {key: null, value: null};
var nullNeigh = {prev: nullEntry, ident: nullEntry, next: nullEntry};

var nt = new haxe.ds.BalancedTree<Int, Int>();
for (k in test.keys()) {
	nt.set(k, test[k]);
}
// no-param versions, get info about whole tree
entryEq({key: 1, value: 4}, nt.floor());
entryEq({key: 1, value: 4}, nt.min());
entryEq({key: 27, value: 10}, nt.ceil());
entryEq({key: 27, value: 10}, nt.max());
neighEq({prev: {key: 1, value: 4}, ident: nullEntry, next: {key: 27, value: 10}}, nt.neighborhood());
// queried key is present, keys before and after
entryEq({key: 11, value: 5}, nt.floor(11));
entryEq({key: 11, value: 5}, nt.ceil(11));
neighEq({prev: {key: 8, value: 2}, ident: {key: 11, value: 5}, next: {key: 13, value: 1}}, nt.neighborhood(11));
// queried key not present, keys before and after
entryEq({key: 17, value: 3}, nt.floor(18));
entryEq({key: 22, value: 9}, nt.ceil(18));
neighEq({prev: {key: 17, value: 3}, ident: nullEntry, next: {key: 22, value: 9}}, nt.neighborhood(18));
// queried key is present, only keys after
entryEq({key: 1, value: 4}, nt.floor(1));
entryEq({key: 1, value: 4}, nt.ceil(1));
neighEq({prev: nullEntry, ident: {key: 1, value: 4}, next: {key: 6, value: 8}}, nt.neighborhood(1));
// queried key not present, only keys after
entryEq(nullEntry, nt.floor(0));
entryEq({key: 1, value: 4}, nt.ceil(0));
neighEq({prev: nullEntry, ident: nullEntry, next: {key: 1, value: 4}}, nt.neighborhood(0));
// queried key is present, only keys before
entryEq({key: 27, value: 10}, nt.floor(27));
entryEq({key: 27, value: 10}, nt.ceil(27));
neighEq({prev: {key: 25, value: 7}, ident: {key: 27, value: 10}, next: nullEntry}, nt.neighborhood(27));
// queried key not present, only keys before
entryEq({key: 27, value: 10}, nt.floor(30));
entryEq(nullEntry, nt.ceil(30));
neighEq({prev: {key: 27, value: 10}, ident: nullEntry, next: nullEntry}, nt.neighborhood(30));

var empty = new haxe.ds.BalancedTree<Int, Int>();
// ensure queries behave properly on an empty tree
entryEq(nullEntry, empty.floor(1));
entryEq(nullEntry, empty.floor());
entryEq(nullEntry, empty.min());
entryEq(nullEntry, empty.ceil(1));
entryEq(nullEntry, empty.ceil());
entryEq(nullEntry, empty.max());
neighEq(nullNeigh, empty.neighborhood(1));
neighEq(nullNeigh, empty.neighborhood());