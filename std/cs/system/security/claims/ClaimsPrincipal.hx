package cs.system.security.claims;

/** An  implementation that supports multiple claims-based identities. */
@:native("System.Security.Claims.ClaimsPrincipal")
extern class ClaimsPrincipal {
	/**
	 * Gets or sets the delegate used to select the claims principal returned by the 
	 * property.
	 * @return The delegate. The default is .
	 */
	static var ClaimsPrincipalSelector(default, default):cs.system.Func_1<cs.system.security.claims.ClaimsPrincipal>;
	/**
	 * Gets the current claims principal.
	 * @return The current claims principal.
	 */
	static var Current(default, never):cs.system.security.claims.ClaimsPrincipal;
	/**
	 * Gets or sets the delegate used to select the claims identity returned by the 
	 * property.
	 * @return The delegate. The default is .
	 */
	static var PrimaryIdentitySelector(default, default):cs.system.Func_2<cs.system.collections.generic.IEnumerable<cs.system.security.claims.ClaimsIdentity>, cs.system.security.claims.ClaimsIdentity>;
	/**
	 * Gets a collection that contains all of the claims from all of the claims
	 * identities associated with this claims principal.
	 * @return The claims associated with this principal.
	 */
	var Claims(default, never):cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim>;
	/**
	 * Contains any additional data provided by a derived type. Typically set when
	 * calling .
	 * @return A  array representing the additional serialized data.
	 */
	var CustomSerializationData(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets a collection that contains all of the claims identities associated with
	 * this claims principal.
	 * @return The collection of claims identities.
	 */
	var Identities(default, never):cs.system.collections.generic.IEnumerable<cs.system.security.claims.ClaimsIdentity>;
	/**
	 * Gets the primary claims identity associated with this claims principal.
	 * @return The primary claims identity associated with this claims principal.
	 */
	var Identity(default, never):cs.system.security.principal.IIdentity;
	@:overload(function():Void {})
	@:overload(function(identities:cs.system.collections.generic.IEnumerable<cs.system.security.claims.ClaimsIdentity>):Void {})
	@:overload(function(reader:cs.system.io.BinaryReader):Void {})
	@:overload(function(identity:cs.system.security.principal.IIdentity):Void {})
	function new(principal:cs.system.security.principal.IPrincipal):Void;
	/**
	 * Adds the specified claims identities to this claims principal.
	 * @param identities The claims identities to add.
	 */
	function AddIdentities(identities:cs.system.collections.generic.IEnumerable<cs.system.security.claims.ClaimsIdentity>):Void;
	/**
	 * Adds the specified claims identity to this claims principal.
	 * @param identity The claims identity to add.
	 */
	function AddIdentity(identity:cs.system.security.claims.ClaimsIdentity):Void;
	/**
	 * Returns a copy of this instance.
	 * @return A new copy of the  object.
	 */
	function Clone():cs.system.security.claims.ClaimsPrincipal;
	@:overload(function(match:cs.system.Predicate<cs.system.security.claims.Claim>):cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim> {})
	/**
	 * Retrieves all of the claims that are matched by the specified predicate.
	 * @param match The function that performs the matching logic.
	 * @return The matching claims.
	 */
	function FindAll(type:String):cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim>;
	@:overload(function(match:cs.system.Predicate<cs.system.security.claims.Claim>):cs.system.security.claims.Claim {})
	/**
	 * Retrieves the first claim that is matched by the specified predicate.
	 * @param match The function that performs the matching logic.
	 * @return The first matching claim or  if no match is found.
	 */
	function FindFirst(type:String):cs.system.security.claims.Claim;
	@:overload(function(match:cs.system.Predicate<cs.system.security.claims.Claim>):Bool {})
	/**
	 * Determines whether any of the claims identities associated with this claims
	 * principal contains a claim that is matched by the specified predicate.
	 * @param match The function that performs the matching logic.
	 * @return if a matching claim exists; otherwise, .
	 */
	function HasClaim(type:String, value:String):Bool;
	/**
	 * Returns a value that indicates whether the entity (user) represented by this
	 * claims principal is in the specified role.
	 * @param role The role for which to check.
	 * @return if claims principal is in the specified role; otherwise, .
	 */
	function IsInRole(role:String):Bool;
	/**
	 * Serializes using a .
	 * @param writer The writer to use for data storage.
	 */
	function WriteTo(writer:cs.system.io.BinaryWriter):Void;
}
