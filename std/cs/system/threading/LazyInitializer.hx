package cs.system.threading;

/** Provides lazy initialization routines. */
@:native("System.Threading.LazyInitializer")
extern class LazyInitializer {
	@:overload(function<T>(target:cs.Ref<T>):T {})
	@:overload(function<T>(target:cs.Ref<T>, valueFactory:cs.system.Func_1<T>):T {})
	@:overload(function<T>(target:cs.Ref<T>, initialized:cs.Ref<Bool>, syncLock:cs.Ref<Dynamic>):T {})
	@:overload(function<T>(target:cs.Ref<T>, syncLock:cs.Ref<Dynamic>, valueFactory:cs.system.Func_1<T>):T {})
	/**
	 * Initializes a target reference type with the type's parameterless constructor if
	 * it hasn't already been initialized.
	 * @param T The type of the reference to be initialized.
	 * @param target A reference to initialize if it has not already been initialized.
	 * @return The initialized object.
	 */
	static function EnsureInitialized<T>(target:cs.Ref<T>, initialized:cs.Ref<Bool>, syncLock:cs.Ref<Dynamic>, valueFactory:cs.system.Func_1<T>):T;
}
