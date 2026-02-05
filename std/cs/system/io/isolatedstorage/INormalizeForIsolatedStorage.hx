package cs.system.io.isolatedstorage;

/** Enables comparisons between an isolated store and an application domain and assembly's evidence. */
@:native("System.IO.IsolatedStorage.INormalizeForIsolatedStorage")
extern interface INormalizeForIsolatedStorage {
	/**
	 * When overridden in a derived class, returns a normalized copy of the object on
	 * which it is called.
	 * @return A normalized object that represents the instance on which this method
	 * was called. This instance can be a string, stream, or any serializable object.
	 */
	function Normalize():Dynamic;
}
