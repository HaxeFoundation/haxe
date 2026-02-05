package cs.system.linq;

@:native("System.Linq.IGrouping")
extern interface IGrouping<TKey, TElement> extends cs.system.collections.generic.IEnumerable<TElement> extends cs.system.collections.IEnumerable {
	var Key(default, never):TKey;
}
