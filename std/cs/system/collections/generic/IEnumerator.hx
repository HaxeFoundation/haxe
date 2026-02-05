package cs.system.collections.generic;

@:native("System.Collections.Generic.IEnumerator")
extern interface IEnumerator<T> extends cs.system.collections.IEnumerator extends cs.system.IDisposable {
	var Current(default, never):T;
}
