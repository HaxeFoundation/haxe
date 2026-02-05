package cs.system.reflection;

/** Provides a mechanism for instantiating proxy objects and handling their method dispatch. */
@:native("System.Reflection.DispatchProxy")
extern class DispatchProxy {
	/**
	 * Creates an object instance that derives from class  and implements interface .
	 * @param T The interface the proxy should implement.
	 * @param TProxy The base class to use for the proxy class.
	 * @return An object instance that implements .
	 */
	static function Create<T, TProxy>():T;
}
