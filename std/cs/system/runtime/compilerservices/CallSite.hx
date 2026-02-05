package cs.system.runtime.compilerservices;

/** A dynamic call site base class. This type is used as a parameter type to the dynamic site targets. */
@:native("System.Runtime.CompilerServices.CallSite")
extern class CallSite {
	/**
	 * Class responsible for binding dynamic operations on the dynamic site.
	 * @return The  object responsible for binding dynamic operations.
	 */
	var Binder(default, never):cs.system.runtime.compilerservices.CallSiteBinder;
	/**
	 * Creates a call site with the given delegate type and binder.
	 * @param delegateType The call site delegate type.
	 * @param binder The call site binder.
	 * @return The new call site.
	 */
	static function Create(delegateType:cs.system.Type, binder:cs.system.runtime.compilerservices.CallSiteBinder):cs.system.runtime.compilerservices.CallSite;
}
