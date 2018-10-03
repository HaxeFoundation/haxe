var l = new List();
l.toString() == "{}";
l.isEmpty() == true;
l.remove("1") == false;
l.length == 0;
l.first() == null;
l.last() == null;
l.pop() == null;
l.popLast() == null;
l.add("1");
l.length == 1;
l.first() == "1";
l.last() == "1";
l.toString() == "{1}";
l.isEmpty() == false;
l.join("x") == "1";
l.pop() == "1";
l.remove("1") == false;
l.length == 0;
l.add("1");
l.length == 1;
l.remove("1") == true;
l.add("1");
l.push("2");
l.length == 2;
l.first() == "2";
l.last() == "1";
l.toString() == "{2, 1}";
l.join("x") == "2x1";
l.clear();
l.isEmpty() == true;
l.add("1");
l.add("2");
l.add("3");
var l2 = l.map(function(i:String) return i + i);
l2.pop() == "11";
l2.popLast() == "33";
l2.pop() == "22";
var l3 = l.filter(function(i:String) return i != "2");
l3.pop() == "1";
l3.popLast() == "3";

// keyValueIterator
var l3 = new List();
l3.add(1);
l3.add(2);
l3.add(3);
l3.add(5);
l3.add(8);
[for (k=>v in l3) k] == [0,1,2,3,4];
[for (k=>v in l3) v] == [1,2,3,5,8];
[for (k=>v in l3) k*v] == [0,2,6,15,32];

l3.clear();

// removeLast
l3.add(0);
l3.removeLast(0) == true;
l3.length == 0;
l3.removeLast(0) == false;
l3.length == 0;
for(i in 0...3) l3.add(i);
l3.add(0);
l3.removeLast(0) == true;
l3.length == 3;
l3.removeLast(9) == false;
l3.length == 3;
l3.first() == 0;
l3.last() == 2;

l3.clear();

// copy
for(i in 0...3) l3.add(i);
var copy = l3.copy();
copy != l3;
[for(i in copy) i] == [0, 1, 2];

l3.clear();

// reverse
l3.reverse();
l3.length == 0;
for(i in 0...3) l3.add(i);
l3.reverse();
[for(i in l3) i] == [2, 1, 0];
l3.first() == 2;
l3.last() == 0;
l3.pop() == 2;
l3.reverse();
[for(i in l3) i] == [0, 1];
l3.popLast() == 1;
[for(i in l3) i] == [0];
l3.reverse();
[for(i in l3) i] == [0];