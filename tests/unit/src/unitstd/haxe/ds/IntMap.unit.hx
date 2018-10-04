var map1 = new haxe.ds.IntMap();
(map1 is haxe.ds.IntMap) == true;
map1.set(1, 2);
map1.set(2, 4);
map1.set(3, 6);
map1.get(1) == 2;
map1.get(2) == 4; 
map1.get(3) != 8;

// iterator
var keys1a = [for (k in map1.keys()) k];
keys1a.sort(Reflect.compare); // Order is undefined
keys1a == [1,2,3];

var values1a = [for (v in map1) v];
values1a.sort(Reflect.compare); 
values1a == [2,4,6];

// key value iterator
var keys1b = [for (k=>v in map1) k];
keys1b.sort(Reflect.compare); 
keys1b == [1,2,3];

var values1b = [for (k=>v in map1) v];
values1b.sort(Reflect.compare); 
values1b == [2,4,6];

var values1c = [for (k=>v in map1) k*v];
values1c.sort(Reflect.compare); 
values1c == [2,8,18];


var map2 = new haxe.ds.IntMap();
(map2 is haxe.ds.IntMap) == true;
map2.set(1, "2");
map2.set(2, "4");
map2.set(3, "6");
map2.get(1) == "2";
map2.get(2) == "4"; 
map2.get(3) != "8";

// iterator
var keys2a = [for (k in map2.keys()) k];
keys2a.sort(Reflect.compare); 
keys2a == [1,2,3];

var values2a = [for (v in map2) v];
values2a.sort(Reflect.compare); 
values2a == ["2","4","6"];
// key value iterator
var keys2b = [for (k=>v in map2) k];
keys2b.sort(Reflect.compare); 
keys2b == [1,2,3];

var values2b = [for (k=>v in map2) v];
values2b.sort(Reflect.compare); 
values2b == ["2","4","6"];