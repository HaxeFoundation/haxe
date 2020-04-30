var map1 = new haxe.ds.StringMap();
(map1 is haxe.ds.StringMap) == true;
map1.set("1a", 2);
map1.set("2a", 4);
map1.set("3a", 6);
map1.get("1a") == 2;
map1.get("2a") == 4; 
map1.get("3a") != 8;

// iterator
var keys1a = [for (k in map1.keys()) k];
keys1a.sort(Reflect.compare); // Order is undefined
keys1a == ["1a","2a","3a"];

var values1a = [for (v in map1) v];
values1a.sort(Reflect.compare); 
values1a == [2,4,6];

// key value iterator
var keys1b = [for (k=>v in map1) k];
keys1b.sort(Reflect.compare); 
keys1b == ["1a","2a","3a"];

var values1b = [for (k=>v in map1) v];
values1b.sort(Reflect.compare); 
values1b == [2,4,6];


var map2 = new haxe.ds.StringMap();
(map2 is haxe.ds.StringMap) == true;
map2.set("1a", "2");
map2.set("2a", "4");
map2.set("3a", "6");
map2.get("1a") == "2";
map2.get("2a") == "4"; 
map2.get("3a") != "8";

// iterator
var keys2a = [for (k in map2.keys()) k];
keys2a.sort(Reflect.compare); 
keys2a == ["1a","2a","3a"];

var values2a = [for (v in map2) v];
values2a.sort(Reflect.compare); 
values2a == ["2","4","6"];
// key value iterator
var keys2b = [for (k=>v in map2) k];
keys2b.sort(Reflect.compare); 
keys2b == ["1a","2a","3a"];

var values2b = [for (k=>v in map2) v];
values2b.sort(Reflect.compare); 
values2b == ["2","4","6"];

// clear
map2.clear();
map2.get("1a") == null;
map2.exists("1a") == false;
map2.exists("2a") == false;
map2.exists("3a") == false;

var keys3 = [for (k in map2.keys()) k];
keys3 == [];

// Test unification

var map:haxe.ds.StringMap<String> = ["1a"=>"2","1b"=>"4"];
var iterable:KeyValueIterable<String, String> = map; 
var iterator:KeyValueIterator<String,String> = iterable.keyValueIterator();
var values = [for(kv in iterator) kv.value];
values[0] in ["2","4"];
values[1] in ["2","4"];
var iterator:KeyValueIterator<String,String> = iterable.keyValueIterator();
var keys = [for(kv in iterator) kv.key];
keys[0] in ["1a", "1b"];
keys[1] in ["1a", "1b"];