#if (target.atomics)
var a = new haxe.atomic.AtomicBool(true);

a.load() == true;
a.store(false) == false;
a.load() == false;

a.compareExchange(false, true) == false;
a.load() == true;

a.compareExchange(false, false) == true;
a.load() == true;

a.exchange(true) == true;
a.load() == true;
#else
0 == 0; // prevent "no assertions" warning
#end
