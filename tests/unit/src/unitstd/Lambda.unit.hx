function name(o:Dynamic) return Type.getClassName(Type.getClass(o));
// array
var a = [1];
var a2 = Lambda.array(a);
name(a) == "Array";
name(a2) == "Array";
a != a2;
a2.length == 1;
a2[0] == 1;
var e = [];
var e2 = Lambda.array(e);
e != e2;
e2.length == 0;

// list
var a = [1];
var l = Lambda.list(a);
name(a) == "Array";
name(l) == "haxe.ds.List";
l.length == 1;
l.first() == 1;
var l2 = Lambda.list(l);
name(l2) == "haxe.ds.List";
l != l2;
var e = new List();
var e2 = Lambda.list(e);
e != e2;
e2.length == 0;

// map
var a = [1, 2, 3];
var b = Lambda.map(a,function(i) return i * 2);
b.length == 3;
b.pop() == 6;
b.pop() == 4;
b.pop() == 2;

// mapi
var a = [1, 2, 3];
var total = 0;
function myMap(index, i) {
	total += index;
	return i * 2;
}
var b = Lambda.mapi(a, myMap);
total == 3;
b.length == 3;
b.pop() == 6;
b.pop() == 4;
b.pop() == 2;

// has
Lambda.has([1,2,3],1) == true;
Lambda.has([1,2,3],4) == false;
Lambda.has([],null) == false;
Lambda.has([null],null) == true;

// exists
Lambda.exists([1, 2, 3], function(i) return i == 2) == true;
Lambda.exists([1, 2, 3], function(i) return i == 4) == false;
Lambda.exists([], function(x) return true) == false;

// foreach
Lambda.foreach([2, 4, 6],function(i) return i % 2 == 0) == true;
Lambda.foreach([2, 4, 7],function(i) return i % 2 == 0) == false;
Lambda.foreach([], function(x) return false) == true;

// iter
var check = 0;
var sum = 0;
Lambda.iter([1,2,3],function(i) {
	if (check != i - 1) throw "no match";
	check = i;
	sum += i;
});
check == 3;
sum == 6;
Lambda.iter([],function(i) return throw "no call");

// filter
Lambda.array(Lambda.filter([1,2,3,4],function(i) return i < 3)) == [1,2];
Lambda.array(Lambda.filter([1,2,3,4],function(i) return true)) == [1,2,3,4];
Lambda.array(Lambda.filter([1,2,3,4],function(i) return false)) == [];
Lambda.array(Lambda.filter([],function(_) return false)) == [];
Lambda.array(Lambda.filter([],function(_) return true)) == [];
Lambda.array(Lambda.filter([],null)) == [];


// fold
Lambda.fold(["b","c","d"],function(s,acc) return s + acc,"a") == "dcba";
Lambda.fold([],function(s:String,acc) return s + acc,"a") == "a";
Lambda.fold([],function(s:String,acc) return s + acc,null) == null;

// foldi
Lambda.foldi(["b","c","d"],function(s,acc,i) return Std.string(i) + s + acc,"a") == "2d1c0ba";
Lambda.foldi([],function(s:String,acc,i) return Std.string(i) + s + acc,"a") == "a";
Lambda.foldi([],function(s:String,acc,i) return Std.string(i) + s + acc,null) == null;

// count
Lambda.count([1,2,3]) == 3;
Lambda.count([1,2,3], function(x) return false) == 0;
Lambda.count([1,2,3], function(x) return true) == 3;
Lambda.count([1,2,3], function(x) return x % 2 == 1) == 2;
Lambda.count([]) == 0;

// empty
Lambda.empty([]) == true;
Lambda.empty([null]) == false;

// indexOf
Lambda.indexOf([1,2,3],1) == 0;
Lambda.indexOf([1,2,3],2) == 1;
Lambda.indexOf([1,2,3],3) == 2;
Lambda.indexOf([1,2,3,3],3) == 2;
Lambda.indexOf([1,2,3],4) == -1;
Lambda.indexOf([],1) == -1;

// find
Lambda.find([1,2,3,4,5],i -> i % 2 == 0) == 2;
Lambda.find([1,2,3,4,5],i -> i % 4 == 0) == 4;
Lambda.find([1,2,3,4,5],i -> i % 8 == 0) == null;
Lambda.find([1,2,3,4,5],i -> true) == 1;
Lambda.find([1,2,3,4,5],i -> false) == null;
Lambda.find([],i -> true) == null;
Lambda.find([],i -> false) == null;

// findIndex
Lambda.findIndex([1,2,3,4,5],i -> i % 2 == 0) == 1;
Lambda.findIndex([1,2,3,4,5],i -> i % 4 == 0) == 3;
Lambda.findIndex([1,2,3,4,5],i -> i % 8 == 0) == -1;
Lambda.findIndex([1,2,3,4,5],i -> true) == 0;
Lambda.findIndex([1,2,3,4,5],i -> false) == -1;
Lambda.findIndex([],i -> true) == -1;
Lambda.findIndex([],i -> false) == -1;

// concat
Lambda.array(Lambda.concat([1,2,3],[3,4,5])) == [1,2,3,3,4,5];
Lambda.array(Lambda.concat([1,2,3],[])) == [1,2,3];
Lambda.array(Lambda.concat([],[1,2,3])) == [1,2,3];
Lambda.array(Lambda.concat([],[])) == [];