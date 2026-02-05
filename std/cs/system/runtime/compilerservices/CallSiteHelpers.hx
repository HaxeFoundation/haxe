package cs.system.runtime.compilerservices;

/** Class that contains helper methods for DLR CallSites. */
@:native("System.Runtime.CompilerServices.CallSiteHelpers")
extern class CallSiteHelpers {
	/**
	 * Checks if a  is internally used by DLR and should not be displayed on the
	 * language code's stack.
	 * @param mb The input
	 * @return True if the input  is internally used by DLR and should not be displayed
	 * on the language code's stack. Otherwise, false.
	 */
	static function IsInternalFrame(mb:cs.system.reflection.MethodBase):Bool;
}
