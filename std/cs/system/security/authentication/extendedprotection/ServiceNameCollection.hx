package cs.system.security.authentication.extendedprotection;

/** The  class is a read-only collection of service principal names. */
@:native("System.Security.Authentication.ExtendedProtection.ServiceNameCollection")
extern class ServiceNameCollection extends cs.system.collections.ReadOnlyCollectionBase {
	function new(items:cs.system.collections.ICollection):Void;
	/**
	 * Returns a value indicating whether the specified string occurs within this 
	 * instance.
	 * @param searchServiceName The string to seek.
	 * @return Returns . if the  parameter occurs within this  instance; otherwise, .
	 */
	function Contains(searchServiceName:String):Bool;
	@:overload(function(serviceNames:cs.system.collections.IEnumerable):cs.system.security.authentication.extendedprotection.ServiceNameCollection {})
	/**
	 * Merges the current  with the specified values to create a new  containing the
	 * union.
	 * @param serviceNames An instance of the  class that contains the specified values
	 * of service names to be merged.
	 * @return A new  instance that contains the union of the existing  instance merged
	 * with the specified values.
	 */
	function Merge(serviceName:String):cs.system.security.authentication.extendedprotection.ServiceNameCollection;
}
