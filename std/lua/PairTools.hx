package lua;
/**
  A set of utility methods for working with the Lua table extern.
 **/
class PairTools {
	public static function ipairsEach<T>(table:Table<Dynamic,T>, func : Int->T->Void) : Void {
		untyped __lua__("for i,v in _G.ipairs(table) do func(i,v) end");
	}
	public static function pairsEach<A,B>(table:Table<A,B>, func : A->B->Void) : Void {
		untyped __lua__("for k,v in _G.pairs(table) do func(k,v) end");
	}
	public static function ipairsMap<A,B>(table:Table<Dynamic,A>, func : Int->A->B) : Table<Int,B>  {
		var ret : Table<Int,B> = cast {};
		untyped __lua__( "for i,v in _G.ipairs(table) do ret[i] = func(i,v) end;");
		return ret;
	}
	public static function pairsMap<A,B,C>(table:Table<A,B>, func : A->B->C->Void) : Table<A,C> {
		var ret : Table<A,C> = cast {};
		untyped __lua__( "for k,v in _G.pairs(table) do ret[k] = func(k,v) end;");
		return ret;
	}
	public static function ipairsFold<A,B>(table:Table<Int,A>, func : Int->A->B->B, seed: B) : B  {
		untyped __lua__("for i,v in _G.ipairs(table) do seed = func(i,v,seed) end");
		return untyped __lua__("seed");
	}
	public static function pairsFold<A,B,C>(table:Table<A,B>, func : A->B->C->C, seed: C) : C {
		untyped __lua__("for k,v in _G.pairs(table) do seed = func(k,v,seed) end");
		return untyped __lua__("seed");
	}
}
