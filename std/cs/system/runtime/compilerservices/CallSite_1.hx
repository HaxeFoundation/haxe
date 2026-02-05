package cs.system.runtime.compilerservices;

/** A dynamic call site base class. This type is used as a parameter type to the dynamic site targets. */
@:native("System.Runtime.CompilerServices.CallSite`1")
extern class CallSite_1<T> extends cs.system.runtime.compilerservices.CallSite {
	var Target:T;
	var Update(default, never):T;
	/**
	 * Creates a call site with the given delegate type and binder.
	 * @param delegateType The call site delegate type.
	 * @param binder The call site binder.
	 * @return The new call site.
	 */
	static function Create<T>(binder:cs.system.runtime.compilerservices.CallSiteBinder):cs.system.runtime.compilerservices.CallSite_1<T>;
}
