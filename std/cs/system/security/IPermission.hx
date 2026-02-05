package cs.system.security;

/** Defines methods implemented by permission types. */
@:native("System.Security.IPermission")
extern interface IPermission extends cs.system.security.ISecurityEncodable {
	/**
	 * Creates and returns an identical copy of the current permission.
	 * @return A copy of the current permission.
	 */
	function Copy():cs.system.security.IPermission;
	/** Throws a  at run time if the security requirement is not met. */
	function Demand():Void;
	/**
	 * Creates and returns a permission that is the intersection of the current
	 * permission and the specified permission.
	 * @param target A permission to intersect with the current permission. It must be
	 * of the same type as the current permission.
	 * @return A new permission that represents the intersection of the current
	 * permission and the specified permission. This new permission is  if the
	 * intersection is empty.
	 */
	function Intersect(target:cs.system.security.IPermission):cs.system.security.IPermission;
	/**
	 * Determines whether the current permission is a subset of the specified
	 * permission.
	 * @param target A permission that is to be tested for the subset relationship.
	 * This permission must be of the same type as the current permission.
	 * @return if the current permission is a subset of the specified permission;
	 * otherwise, .
	 */
	function IsSubsetOf(target:cs.system.security.IPermission):Bool;
	/**
	 * Creates a permission that is the union of the current permission and the
	 * specified permission.
	 * @param target A permission to combine with the current permission. It must be of
	 * the same type as the current permission.
	 * @return A new permission that represents the union of the current permission and
	 * the specified permission.
	 */
	function Union(target:cs.system.security.IPermission):cs.system.security.IPermission;
}
