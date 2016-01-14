package cpp;

extern class VirtualArray
{
   function new() : Void;
   var length(get,null) : Int;
   // concat<T>( a:Array<T> ) : Array<T> ?
   function concat( a : VirtualArray ) : VirtualArray;
   function join( sep : String ) : String;
   function pop() : Dynamic;
   function push(x : Dynamic) : Int;
   function reverse() : Void;
   function shift() : Dynamic;
   function slice( pos : Int, ?end : Int ) : VirtualArray;
   function sort( f : Dynamic -> Dynamic -> Int ) : Void;
   function splice( pos : Int, len : Int ) : VirtualArray;
   function toString() : String;
   function unshift( x : Dynamic ) : Void;
   function insert( pos : Int, x : Dynamic ) : Void;
   function remove( x : Dynamic ) : Bool;
   function indexOf( x : Dynamic, ?fromIndex:Int ) : Int;
   function lastIndexOf( x : Dynamic, ?fromIndex:Int ) : Int;
   function copy() : VirtualArray;
   function iterator() : Iterator<Dynamic>;
   function map<S>( f : Dynamic -> S ) : VirtualArray;
   function filter( f : Dynamic -> Bool ) : VirtualArray;
}



