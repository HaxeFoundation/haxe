package cs.system.security.claims;

/** Represents a claims-based identity. */
@:native("System.Security.Claims.ClaimsIdentity")
extern class ClaimsIdentity {
	/** The default issuer; "LOCAL AUTHORITY". */
	static var DefaultIssuer(default, never):String;
	/** The default name claim type; . */
	static var DefaultNameClaimType(default, never):String;
	/** The default role claim type; . */
	static var DefaultRoleClaimType(default, never):String;
	/**
	 * Gets or sets the identity of the calling party that was granted delegation
	 * rights.
	 * @return The calling party that was granted delegation rights.
	 */
	var Actor(default, default):cs.system.security.claims.ClaimsIdentity;
	/**
	 * Gets the authentication type.
	 * @return The authentication type.
	 */
	var AuthenticationType(default, never):String;
	/**
	 * Gets or sets the token that was used to create this claims identity.
	 * @return The bootstrap context.
	 */
	var BootstrapContext(default, default):Dynamic;
	/**
	 * Gets the claims associated with this claims identity.
	 * @return The collection of claims associated with this claims identity.
	 */
	var Claims(default, never):cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim>;
	/**
	 * Contains any additional data provided by a derived type. Typically set when
	 * calling .
	 * @return A  array representing the additional serialized data.
	 */
	var CustomSerializationData(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets a value that indicates whether the identity has been authenticated.
	 * @return if the identity has been authenticated; otherwise, .
	 */
	var IsAuthenticated(default, never):Bool;
	/**
	 * Gets or sets the label for this claims identity.
	 * @return The label.
	 */
	var Label(default, default):String;
	/**
	 * Gets the name of this claims identity.
	 * @return The name or .
	 */
	var Name(default, never):String;
	/**
	 * Gets the claim type that is used to determine which claims provide the value for
	 * the  property of this claims identity.
	 * @return The name claim type.
	 */
	var NameClaimType(default, never):String;
	/**
	 * Gets the claim type that will be interpreted as a .NET Framework role among the
	 * claims in this claims identity.
	 * @return The role claim type.
	 */
	var RoleClaimType(default, never):String;
	@:overload(function():Void {})
	@:overload(function(claims:cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim>):Void {})
	@:overload(function(reader:cs.system.io.BinaryReader):Void {})
	@:overload(function(identity:cs.system.security.principal.IIdentity):Void {})
	@:overload(function(authenticationType:String):Void {})
	@:overload(function(claims:cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim>, authenticationType:String):Void {})
	@:overload(function(identity:cs.system.security.principal.IIdentity, claims:cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim>):Void {})
	@:overload(function(authenticationType:String, nameType:String, roleType:String):Void {})
	@:overload(function(claims:cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim>, authenticationType:String, nameType:String, roleType:String):Void {})
	function new(identity:cs.system.security.principal.IIdentity, claims:cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim>, authenticationType:String, nameType:String, roleType:String):Void;
	/**
	 * Adds a single claim to this claims identity.
	 * @param claim The claim to add.
	 */
	function AddClaim(claim:cs.system.security.claims.Claim):Void;
	/**
	 * Adds a list of claims to this claims identity.
	 * @param claims The claims to add.
	 */
	function AddClaims(claims:cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim>):Void;
	/**
	 * Returns a new  copied from this claims identity.
	 * @return A copy of the current instance.
	 */
	function Clone():cs.system.security.claims.ClaimsIdentity;
	@:overload(function(match:cs.system.Predicate<cs.system.security.claims.Claim>):cs.system.collections.generic.IEnumerable<cs.system.security.claims.Claim> {})
	/**
	 * Retrieves all of the claims that are matched by the specified predicate.
	 * @param match The function that performs the matching logic.
	 * @return The matching claims. The list is read-only.
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
	 * Determines whether this claims identity has a claim that is matched by the
	 * specified predicate.
	 * @param match The function that performs the matching logic.
	 * @return if a matching claim exists; otherwise, .
	 */
	function HasClaim(type:String, value:String):Bool;
	/**
	 * Attempts to remove a claim from the claims identity.
	 * @param claim The claim to remove.
	 */
	function RemoveClaim(claim:cs.system.security.claims.Claim):Void;
	/**
	 * Attempts to remove a claim from the claims identity.
	 * @param claim The claim to remove.
	 * @return if the claim was successfully removed; otherwise, .
	 */
	function TryRemoveClaim(claim:cs.system.security.claims.Claim):Bool;
	/**
	 * Serializes using a .
	 * @param writer The writer to use for data storage.
	 */
	function WriteTo(writer:cs.system.io.BinaryWriter):Void;
}
