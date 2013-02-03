var ii = new IntIterator(0, 2);
ii.hasNext() == true;
ii.next() == 0;
ii.hasNext() == true;
ii.next() == 1;
ii.hasNext() == false;
var ii = new IntIterator(0, 2);
var r = [];
for (i in ii)
	r.push(i);
r == [1, 2];
for (i in ii)
	r.push(i);
r == [1, 2];