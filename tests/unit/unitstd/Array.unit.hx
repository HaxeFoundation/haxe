// length
[].length == 0;
[1].length == 1;
var a = [];
a[4] = 1;
a.length == 5;

// concat
[].concat([]) == [];
[1].concat([]) == [1];
[].concat([1]) == [1];
[1].concat([2]) == [1,2];
[1,2].concat([2,1]) == [1,2,2,1];

// join
[1,2].join("") == "12";
[].join("x") == "";
[1].join("x") == "1";
[1,2].join("x") == "1x2";
[].join("") == "";
[new ClassWithToString(), new ClassWithToStringChild(), new ClassWithToStringChild2()].join("_") == "ClassWithToString.toString()_ClassWithToString.toString()_ClassWithToStringChild2.toString()";

// pop
[].pop() == null;
[1].pop() == 1;
var a = [1, 2, 3];
var b = a;
a.pop() == 3;
a == [1, 2];
a == b;
a.pop() == 2;
a == [1];
a == b;
a.pop() == 1;
a == [];
a == b;
a.pop() == null;
a == [];
a == b;

// push
var a:Array<Null<Int>> = [];
var b = a;
a.push(1) == 1;
a == b;
a == [1];
a.push(2) == 2;
a == b;
a == [1, 2];
a.push(null) == 3;
a == [1, 2, null];

// reverse
var a = [1, 2, 3];
var b = a;
a.reverse();
a == b;
a == [3, 2, 1];
var a = [];
a.reverse();
a == [];
var a = [1];
a.reverse();
a == [1];

// shift
[].shift() == null;
[1].shift() == 1;
var a = [1, 2, 3];
var b = a;
a.shift() == 1;
a == [2, 3];
a == b;
a.shift() == 2;
a == [3];
a == b;
a.shift() == 3;
a == [];
a == b;
a.shift() == null;
a == [];
a == b;

// slice
var i0 = new IntWrap(1);
var i1 = new IntWrap(1);
var i2 = new IntWrap(5);
var i3 = new IntWrap(9);
var i4 = new IntWrap(2);
var a = [i4,i0,i1,i3,i0,i2];
var b = a.slice(0);
b != a;
b == [i4, i0, i1, i3, i0, i2];
b = b.slice(1);
b == [i0, i1, i3, i0, i2];
b = b.slice(1, 3);
b == [i1, i3];
b = b.slice( -1);
b == [i3];
b = b.slice(0, 4);
b == [i3];
b.slice( -3) == [i3];
b.slice( -3, -3) == [];
[1, 2, 3].slice(2, 1) == [];

// sort
var i0 = new IntWrap(1);
var i1 = new IntWrap(1);
var i2 = new IntWrap(5);
var i3 = new IntWrap(9);
var i4 = new IntWrap(2);
var a = [i4, i0, i1, i3, i0, i2];
a.sort(IntWrap.compare);
a == [i0, i1, i0, i4, i2, i3];

// splice
var i0 = new IntWrap(1);
var i1 = new IntWrap(1);
var i2 = new IntWrap(5);
var i3 = new IntWrap(9);
var i4 = new IntWrap(2);
var b = [i4, i0, i1, i3, i0, i2];
var a = b.splice(0, 0);
b != a;
a == [];
b == [i4, i0, i1, i3, i0, i2];
a = b.splice(1, b.length - 1);
b == [i4];
a == [i0, i1, i3, i0, i2];
b = a.splice(1, -1);
a == [i0, i1, i3, i0, i2];
b == [];
b = a.splice(0, 10);
b == [i0, i1, i3, i0, i2];
a == [];
a = b.splice(10, 10);
a == [];
b = [i0, i1, i3, i0, i2];
a = b.splice( -2, 2);
b == [i0, i1, i3];
a == [i0, i2];

// toString
var a = [new ClassWithToString(), new ClassWithToStringChild(), new ClassWithToStringChild2()];
var comp = "ClassWithToString.toString(),ClassWithToString.toString(),ClassWithToStringChild2.toString()";
a.toString() in [comp, "[" + comp + "]"];

// unshift
var a:Array<Null<Int>> = [];
var b = a;
a.unshift(1);
a == b;
a == [1];
a.unshift(2);
a == b;
a == [2, 1];
a.unshift(null);
a == [null, 2, 1];

// insert
var a = [];
a.insert(5, 1);
a == [1];
var a = [1, 2, 3];
a.insert(1, 4);
a == [1, 4, 2, 3];
var a = [1, 2, 3];
a.insert( -1, 4);
a == [1, 2, 4, 3];
a.insert( -2, 8);
a == [1, 2, 8, 4, 3];
a.insert ( -8, 9);
a == [9, 1, 2, 8, 4, 3];

// remove
var i0 = new IntWrap(1);
var i1 = new IntWrap(1);
var i2 = new IntWrap(5);
var i3 = new IntWrap(9);
var i4 = new IntWrap(2);
var a = [i4, i0, i1, i3, i0, i2];
a.remove(i0) == true;
a == [i4, i1, i3, i0, i2];
a.remove(i0) == true;
a == [i4, i1, i3, i2];
a.remove(i0) == false;
a == [i4, i1, i3, i2];
var a = ["foo", "bar"];
a.remove("foo") == true;
a == ["bar"];
var a = [i0, null, i1, null, null];
a.remove(null) == true;
a == [i0, i1, null, null];
a.remove(null) == true;
a == [i0, i1, null];
a.remove(null) == true;
a == [i0, i1];
a.remove(null) == false;
a == [i0, i1];

// copy
var i0 = new IntWrap(1);
var i1 = new IntWrap(1);
var i2 = new IntWrap(5);
var a = [i0, i1, i2];
var b = a.copy();
a != b;
b == [i0, i1, i2];
var a = [];
var b = a.copy();
a != b;
b == [];