package cs.system.collections.generic;

@:native("System.Collections.Generic.IReadOnlyCollection")
extern interface IReadOnlyCollection<T> extends cs.system.collections.generic.IEnumerable<T> extends cs.system.collections.IEnumerable {
	var Count(default, never):Int;
}
