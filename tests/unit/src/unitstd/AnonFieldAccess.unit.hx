([1]:Iterable<Int>).iterator().next() == 1;
([1]:{function shift ():Null<Int>;}).shift() == 1;
([1]:{function pop ():Null<Int>;}).pop() == 1;
([]:{function push (x:Int):Int;}).push(1) == 1;
([1,2]:{function join (s:String):String;}).join(",") == "1,2";
([1,2]:{function filter (s:Int->Bool):Array<Int>;}).filter(function (x) return x == 1).length == 1;
([1]:{function map <T>(s:Int->T):Array<T>;}).map(function (x) return x+1)[0] == 2;
("a":{function toUpperCase ():String;}).toUpperCase() == "A";
("A":{function toLowerCase ():String;}).toLowerCase() == "a";


