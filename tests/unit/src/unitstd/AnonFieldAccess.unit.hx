([1]:Iterable<Int>).iterator().next() == 1;
([1]:{function shift ():Null<Int>;}).shift() == 1;
([1]:{function pop ():Null<Int>;}).pop() == 1;
([]:{function push (x:Int):Int;}).push(1) == 1;
([1,2]:{function join (s:String):String;}).join(",") == "1,2";
([1,2]:{function filter (s:Int->Bool):Array<Int>;}).filter(function (x) return x == 1).length == 1;
([1]:{function map <T>(s:Int->T):Array<T>;}).map(function (x) return x+1)[0] == 2;

/*

The following array methods are not available at runtime,
we should think about this, because it seems quite arbitrary.

([1]:{function slice (x:Int,y:Int):Array<Int>;}).slice(0,1).join(",") == "1";
([1]:{function splice (x:Int,y:Int):Array<Int>;}).splice(0,1).join(",") == "1";
([1]:{function concat (x:Array<Int>):Array<Int>;}).concat([2]).join(",") == "1,2";
([1]:{function indexOf (x:Int, ?fromIndex : Null<Int>):Int;}).indexOf(1) == 0;
([1]:{function lastIndexOf (x:Int, ?fromIndex : Null<Int>):Int;}).lastIndexOf(1) == 0;
([1]:{function remove (x:Int):Bool;}).remove(1) == true;
([1,2]:{function toString ():String;}).toString() == "1,2";
([1]:{function copy ():Array<Int>;}).copy().join(",") == "1";

{
	var x = [1,2];
	var a = (x:{function unshift (x:Int):Void;});
	a.unshift(0);
	x.join(",") == "1";
}
{
	var x = [];
	var a = (x:{function insert (pos:Int, x:Int):Void;});
	a.insert(0,1);
	x.join(",") == "1";
}
{
	var x = [2,1];
	var a = (x:{function sort (x:Int->Int->Int):Void;});
	a.sort(function (a,b) return a-b);
	x.join(",") == "1,2";
}
{
	var x = [1,2];
	var a = (x:{function reverse ():Void;});
	a.reverse();
	x.join(",") == "2,1";
}
*/