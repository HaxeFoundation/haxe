#if (!js && !cpp)
var a = new haxe.atomic.AtomicObject("Hey World!");

a.load() == "Hey World!";
a.store("Hello World!") == "Hello World!";
a.load() == "Hello World!";

a.compareExchange("Hello World!", "Goodbye World!") == "Hello World!";
a.load() == "Goodbye World!";

a.exchange("Hello World!") == "Goodbye World!";
a.load() == "Hello World!";
#end
