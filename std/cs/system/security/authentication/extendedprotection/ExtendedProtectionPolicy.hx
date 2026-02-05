package cs.system.security.authentication.extendedprotection;

/** The  class represents the extended protection policy used by the server to validate incoming client connections. */
@:native("System.Security.Authentication.ExtendedProtection.ExtendedProtectionPolicy")
extern class ExtendedProtectionPolicy {
	/**
	 * Indicates whether the operating system supports integrated windows
	 * authentication with extended protection.
	 * @return if the operating system supports integrated windows authentication with
	 * extended protection, otherwise .
	 */
	static var OSSupportsExtendedProtection(default, never):Bool;
	/**
	 * Gets a custom channel binding token (CBT) to use for validation.
	 * @return A  that contains a custom channel binding to use for validation.
	 */
	var CustomChannelBinding(default, never):cs.system.security.authentication.extendedprotection.ChannelBinding;
	/**
	 * Gets the custom Service Provider Name (SPN) list used to match against a
	 * client's SPN.
	 * @return A  that contains the custom SPN list that is used to match against a
	 * client's SPN.
	 */
	var CustomServiceNames(default, never):cs.system.security.authentication.extendedprotection.ServiceNameCollection;
	/**
	 * Gets when the extended protection policy should be enforced.
	 * @return A  value that indicates when the extended protection policy should be
	 * enforced.
	 */
	var PolicyEnforcement(default, never):cs.system.security.authentication.extendedprotection.PolicyEnforcement;
	/**
	 * Gets the kind of protection enforced by the extended protection policy.
	 * @return A  value that indicates the kind of protection enforced by the policy.
	 */
	var ProtectionScenario(default, never):cs.system.security.authentication.extendedprotection.ProtectionScenario;
	@:overload(function(policyEnforcement:cs.system.security.authentication.extendedprotection.PolicyEnforcement):Void {})
	@:overload(function(policyEnforcement:cs.system.security.authentication.extendedprotection.PolicyEnforcement, customChannelBinding:cs.system.security.authentication.extendedprotection.ChannelBinding):Void {})
	@:overload(function(policyEnforcement:cs.system.security.authentication.extendedprotection.PolicyEnforcement, protectionScenario:cs.system.security.authentication.extendedprotection.ProtectionScenario, customServiceNames:cs.system.collections.ICollection):Void {})
	function new(policyEnforcement:cs.system.security.authentication.extendedprotection.PolicyEnforcement, protectionScenario:cs.system.security.authentication.extendedprotection.ProtectionScenario, customServiceNames:cs.system.security.authentication.extendedprotection.ServiceNameCollection):Void;
	/**
	 * Gets a string representation for the extended protection policy instance.
	 * @return A  instance that contains the representation of the  instance.
	 */
	function ToString():String;
}
