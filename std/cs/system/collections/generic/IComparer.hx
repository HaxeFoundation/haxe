package cs.system.collections.generic;

@:native("System.Collections.Generic.IComparer")
extern interface IComparer<T> {
	function Compare(x:T, y:T):Int;
}
