#if target.atomics
var a = new haxe.atomic.AtomicInt(0);

a.load() == 0;

a.store(5) == 5;
a.load() == 5;

a.add(5) == 5;
a.load() == 10;

a.sub(5) == 10;
a.load() == 5;

a.and(20) == 5;
a.load() == 4;

a.or(3) == 4;
a.load() == 7;

a.xor(2) == 7;
a.load() == 5;

a.compareExchange(0, 0) == 5;
a.load() == 5;
a.compareExchange(5, 0) == 5;
a.load() == 0;

a.exchange(10) == 0;
a.load() == 10;
#end