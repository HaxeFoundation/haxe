var map:Map<String, Int> = new Map();
var romap:haxe.ds.ReadOnlyMap<String, Int> = map;

map['foo'] = 1;

romap.exists('foo') == true;
romap['foo'] == 1;

map['foo'] = 2;
romap['foo'] == 2;

romap.exists('bar') == false;
romap['bar'] == null;

[for(k in romap.keys()) k] == ['foo'];
[for(k in romap.keys()) k] == ['foo'];
[for(v in romap) v] == [2];
[for(k => v in romap) k + v] == ['foo2'];
Lambda.count(romap) == 1;


var map:Map<Int, Int> = new Map();
var romap:haxe.ds.ReadOnlyMap<Int, Int> = map;

map[64] = 1;

romap.exists(64) == true;
romap[64] == 1;

map[64] = 2;
romap[64] == 2;

romap.exists(65) == false;
romap[65] == null;

[for(k in romap.keys()) k] == [64];
[for(k in romap.keys()) k] == [64];
[for(v in romap) v] == [2];
[for(k => v in romap) k + v] == [66];
Lambda.count(romap) == 1;
