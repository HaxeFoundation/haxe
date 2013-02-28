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
name(l) == "List";
l.length == 1;
l.first() == 1;
var l2 = Lambda.list(l);
name(l2) == "List";
l != l2;
var e = new List();
var e2 = Lambda.list(e);
e != e2;
e2.length == 0;

// map
var a = [1, 2, 3];
var b = Lambda.map(a,function(i) return i * 2);
b.length == 3;
b.pop() == 2;
b.pop() == 4;
b.pop() == 6;

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
b.pop() == 2;
b.pop() == 4;
b.pop() == 6;

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
Lambda.array(Lambda.filter([],function(i) return false)) == [];
Lambda.array(Lambda.filter([],function(i) return true)) == [];
Lambda.array(Lambda.filter([],null)) == [];

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

// concat
Lambda.array(Lambda.concat([1,2,3],[3,4,5])) == [1,2,3,3,4,5];
Lambda.array(Lambda.concat([1,2,3],[])) == [1,2,3];
Lambda.array(Lambda.concat([],[1,2,3])) == [1,2,3];
Lambda.array(Lambda.concat([],[])) == [];