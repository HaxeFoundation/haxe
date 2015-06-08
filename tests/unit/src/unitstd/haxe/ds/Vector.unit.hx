var vec = new haxe.ds.Vector(3);
var vNullInt = #if (flash || cpp || java || cs) 0 #else null #end;
var vNullBool = #if (flash || cpp || java || cs) false #else null #end;
var vNullFloat = #if (flash || cpp || java || cs) 0.0 #else null #end;

vec.length == 3;
vec.get(0) == vNullInt;
vec.get(1) == vNullInt;
vec.get(2) == vNullInt;
vec.set(1, 2);
vec.length == 3;
vec.get(0) == vNullInt;
vec.get(1) == 2;
vec.get(2) == vNullInt;

// float init
var vec = new haxe.ds.Vector<Float>(3);
vec.get(0) == vNullFloat;
vec.get(1) == vNullFloat;
vec.get(2) == vNullFloat;

// bool init
// Adobe's compilers seem to have a bug here that gives null instead of false
#if !as3
var vec = new haxe.ds.Vector<Bool>(3);
vec.get(0) == vNullBool;
vec.get(1) == vNullBool;
vec.get(2) == vNullBool;
#end

// fromArray
var arr = ["1", "2", "3"];
var vec:haxe.ds.Vector<String> = haxe.ds.Vector.fromArrayCopy(arr);
#if (!flash && !neko && !cs && !java)
arr != vec.toData();
#end
vec.length == 3;
vec.get(0) == "1";
vec.get(1) == "2";
vec.get(2) == "3";

// objects
var tpl = new C();
var vec:haxe.ds.Vector<C> = haxe.ds.Vector.fromArrayCopy([tpl]);
tpl == vec.get(0);

// toData + fromData
var vec:haxe.ds.Vector<String> = haxe.ds.Vector.fromArrayCopy(["1", "2", "3"]);
var data = vec.toData();
var vec2 = haxe.ds.Vector.fromData(data);
vec2.get(0) == "1";
vec2.get(1) == "2";
vec2.get(2) == "3";

// []
vec2[0] == "1";
vec2[1] == "2";
vec2[2] == "3";
vec2[1] = "4";
vec2[1] == "4";
vec2[0] += "a";
vec2[0] = "1a";

// blit
var vec3 = haxe.ds.Vector.fromArrayCopy([0,1,2,3,4,5,6]);
var vec4 = new haxe.ds.Vector(5);

haxe.ds.Vector.blit(vec3, 0, vec4, 1, 3);
vec4[1] == 0;
vec4[2] == 1;
vec4[3] == 2;
vec4[4] == vNullInt;
vec4[0] == vNullInt;

haxe.ds.Vector.blit(vec3, 0, vec4, 0, 5);
vec4[0] == 0;
vec4[1] == 1;
vec4[2] == 2;
vec4[3] == 3;
vec4[4] == 4;

haxe.ds.Vector.blit(vec4, 1, vec3, 0, 4);
//vec3 should be [1,2,3,4,4,5,6]
vec3[0] == 1;
vec3[1] == 2;
vec3[2] == 3;
vec3[3] == 4;
vec3[4] == 4;
vec3[5] == 5;
vec3[6] == 6;

// test iteration

var vec1 = new haxe.ds.Vector(2);
vec1[0] = 1;
vec1[1] = 2;
var res = 0;
for (e in vec1) {
	res += e;
}
eq(3, res);

// copy

#if (!python && !cs)
var i0 = new IntWrap(1);
var i1 = new IntWrap(1);
var i2 = new IntWrap(5);

var vec = new haxe.ds.Vector(3);
vec[0] = i0;
vec[1] = i1;
vec[2] = i2;
var vec2 = vec.copy();
f(vec == vec2);
vec[0] == vec2[0];
vec[1] == vec2[1];
vec[2] == vec2[2];

// join

var vec = new haxe.ds.Vector(0);
vec.join(",") == "";

var vec = new haxe.ds.Vector(1);
vec.join(",") == "null";

var vec = new haxe.ds.Vector(2);
vec.join(",") == "null,null";

var vec = new haxe.ds.Vector(2);
vec[0] = "foo";
vec[1] = "bar";
vec.join(", ") == "foo, bar";


// map

//var vec = new haxe.ds.Vector(0);
//vec.map(function(i) return throw false);
//
//var vec = new haxe.ds.Vector(2);
//vec[0] = 12;
//vec[1] = 13;
//var vec2 = vec.map(function(i) return "value: " +i);
//vec2[0] == "value: 12";
//vec2[1] == "value: 13";

// sort

#if !(neko || cs || java)
var vec = new haxe.ds.Vector(4);
vec[0] = 99;
vec[1] = 101;
vec[2] = -12;
vec[3] = 0;
vec.sort(Reflect.compare);
vec[0] == -12;
vec[1] == 0;
vec[2] == 99;
vec[3] == 101;
#end

#end