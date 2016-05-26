package cpp;

@:native("cpp::VirtualArray")
@:coreType extern class NativeVirtualArray implements ArrayAccess<Dynamic>
{
   public function new() : Void;
   public var length(get,null) : Int;
   // concat<T>( a:Array<T> ) : Array<T> ?
   public function concat( a : VirtualArray ) : VirtualArray;
   public function join( sep : String ) : String;
   public function pop() : Dynamic;
   public function push(x : Dynamic) : Int;
   public function reverse() : Void;
   public function shift() : Dynamic;
   public function slice( pos : Int, ?end : Int ) : VirtualArray;
   public function sort( f : Dynamic -> Dynamic -> Int ) : Void;
   public function splice( pos : Int, len : Int ) : VirtualArray;
   public function toString() : String;
   public function unshift( x : Dynamic ) : Void;
   public function insert( pos : Int, x : Dynamic ) : Void;
   public function remove( x : Dynamic ) : Bool;
   public function indexOf( x : Dynamic, ?fromIndex:Int ) : Int;
   public function lastIndexOf( x : Dynamic, ?fromIndex:Int ) : Int;
   public function copy() : VirtualArray;
   public function iterator() : Iterator<Dynamic>;
   public function map<S>( f : Dynamic -> S ) : VirtualArray;
   public function filter( f : Dynamic -> Bool ) : VirtualArray;
}


abstract VirtualArray(NativeVirtualArray)
{
   // Add these two functions...
   @:from @:extern inline static public function fromArray<T>(a:Array<T>) : VirtualArray
      return untyped a;
   @:to @:extern inline public function toArray<T>() : Array<T>
      return untyped this;




   // The rest is just boiler-plate
   inline public function new() this=new NativeVirtualArray();

   @:extern @:arrayAccess inline function get(idx:Int) : Dynamic
      return untyped this[idx];

   @:extern @:arrayAccess inline function set<T>(pos:Int, value:T ) : T
      return untyped this[idx] = value;

   public var length(get,never) : Int;
   @:extern inline public function get_length() : Int return this.length;



   // concat<T>( a:Array<T> ) : Array<T> ?
   @:extern inline public function concat( a : VirtualArray ) : VirtualArray return this.concat(a);
   @:extern inline public function join( sep : String ) : String return this.join(sep);
   @:extern inline public function pop() : Dynamic return this.pop();
   @:extern inline public function push(x : Dynamic) : Int return this.push(x);
   @:extern inline public function reverse() : Void this.reverse();
   @:extern inline public function shift() : Dynamic return this.shift();
   @:extern inline public function slice( pos : Int, ?end : Int ) : VirtualArray return this.slice(pos,end);
   @:extern inline public function sort( f : Dynamic -> Dynamic -> Int ) : Void this.sort(f);
   @:extern inline public function splice( pos : Int, len : Int ) : VirtualArray return this.slice(pos,len);
   @:extern inline public function unshift( x : Dynamic ) : Void this.unshift(x);
   @:extern inline public function insert( pos : Int, x : Dynamic ) : Void this.insert(pos,x);
   @:extern inline public function remove( x : Dynamic ) : Bool return this.remove(x);
   @:extern inline public function indexOf( x : Dynamic, ?fromIndex:Int ) : Int return this.indexOf(x,fromIndex);
   @:extern inline public function lastIndexOf( x : Dynamic, ?fromIndex:Int ) : Int return this.lastIndexOf(x,fromIndex);
   @:extern inline public function copy() : VirtualArray return this.copy();
   @:extern inline public function iterator() : Iterator<Dynamic> return this.iterator();
   @:extern inline public function map<S>( f : Dynamic -> S ) : VirtualArray return this.map(f);
   @:extern inline public function filter( f : Dynamic -> Bool ) : VirtualArray return this.filter(f);
}

