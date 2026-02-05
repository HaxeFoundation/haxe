package cs.system.security.permissions;

/** Allows security actions for  to be applied to code using declarative security. This class cannot be inherited. */
@:native("System.Security.Permissions.SecurityPermissionAttribute")
extern class SecurityPermissionAttribute extends cs.system.security.permissions.CodeAccessSecurityAttribute {
	/**
	 * Gets or sets a value indicating whether permission to assert that all this
	 * code's callers have the requisite permission for the operation is declared.
	 * @return if permission to assert is declared; otherwise, .
	 */
	var Assertion(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether code has permission to perform
	 * binding redirection in the application configuration file.
	 * @return if code can perform binding redirects; otherwise, .
	 */
	var BindingRedirects(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether permission to manipulate  is declared.
	 * @return if permission to manipulate  is declared; otherwise, .
	 */
	var ControlAppDomain(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether permission to alter or manipulate domain
	 * security policy is declared.
	 * @return if permission to alter or manipulate security policy in an application
	 * domain is declared; otherwise, .
	 */
	var ControlDomainPolicy(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether permission to alter or manipulate
	 * evidence is declared.
	 * @return if the ability to alter or manipulate evidence is declared; otherwise, .
	 */
	var ControlEvidence(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether permission to view and manipulate
	 * security policy is declared.
	 * @return if permission to manipulate security policy is declared; otherwise, .
	 */
	var ControlPolicy(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether permission to manipulate the current
	 * principal is declared.
	 * @return if permission to manipulate the current principal is declared;
	 * otherwise, .
	 */
	var ControlPrincipal(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether permission to manipulate threads is
	 * declared.
	 * @return if permission to manipulate threads is declared; otherwise, .
	 */
	var ControlThread(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether permission to execute code is declared.
	 * @return if permission to execute code is declared; otherwise, .
	 */
	var Execution(default, default):Bool;
	/**
	 * Gets or sets all permission flags comprising the  permissions.
	 * @return One or more of the  values combined using a bitwise OR.
	 */
	var Flags(default, default):cs.system.security.permissions.SecurityPermissionFlag;
	/**
	 * Gets or sets a value indicating whether code can plug into the common language
	 * runtime infrastructure, such as adding Remoting Context Sinks, Envoy Sinks and
	 * Dynamic Sinks.
	 * @return if code can plug into the common language runtime infrastructure;
	 * otherwise, .
	 */
	var Infrastructure(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether code can configure remoting types and
	 * channels.
	 * @return if code can configure remoting types and channels; otherwise, .
	 */
	var RemotingConfiguration(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether code can use a serialization formatter
	 * to serialize or deserialize an object.
	 * @return if code can use a serialization formatter to serialize or deserialize an
	 * object; otherwise, .
	 */
	var SerializationFormatter(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether permission to bypass code verification
	 * is declared.
	 * @return if permission to bypass code verification is declared; otherwise, .
	 */
	var SkipVerification(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether permission to call unmanaged code is
	 * declared.
	 * @return if permission to call unmanaged code is declared; otherwise, .
	 */
	var UnmanagedCode(default, default):Bool;
	function new(action:cs.system.security.permissions.SecurityAction):Void;
	/**
	 * Creates and returns a new .
	 * @return A  that corresponds to this attribute.
	 */
	function CreatePermission():cs.system.security.IPermission;
}
