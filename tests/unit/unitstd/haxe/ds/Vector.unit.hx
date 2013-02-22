var vec = new haxe.ds.Vector(3);
var vNullInt = #if (flash9 || cpp || java || cs) 0 #else null #end;
var vNullBool = #if (flash9 || cpp || java || cs) false #else null #end;
var vNullFloat = #if (flash9 || cpp || java || cs) 0.0 #else null #end;

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

/*
// grow
vec2 = vec2.grow(15);
vec2.length == 15;
(vec2[3] = "4") == "4";
(vec2[4] = "5") == "5";
vec2[0] == "1a";
vec2[3] == "4";
vec2[4] == "5";
(vec2[10] = "11") == "11";
vec2[10] == "11";

vec2 = vec2.grow(2);
vec2.length == 2;
vec2[0] == "1a";
vec2[1] == "4";

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

// unsafe get / set
vec3.unsafeGet(0) == 1;
vec3.unsafeGet(1) == 2;
vec3.unsafeGet(2) == 3;
vec3.unsafeGet(3) == 4;

vec3.unsafeSet(0, 10);
vec3[0] == 10;
vec3.unsafeGet(0) == 10;
vec3.unsafeGet(6) == 6;
vec3.unsafeSet(6, 10);
vec3.unsafeGet(6) == 10;
*/
