package lua;
/**
  Abstract implementation of a standard Lua table.  Automatically converts to the
  1-based indexing that Lua uses as default for int-keyed tables.
 **/
abstract Table<A,B>({}) {
	inline public function new() this = {};
	@:arrayAccess inline function getIntV(k:Int) : B{
		return untyped this[k+1];
	}
	@:arrayAccess inline function setIntV(k:Int, v:B){
		untyped this[k+1] = v;
		return v;
	}
	@:arrayAccess inline function getV(k:A) : B{
		return untyped this[k];
	}
	@:arrayAccess inline function setV(k:A, v:B){
		untyped this[k] = v;
		return v;
	}
}

