package cs.system.net;

/** Provides credentials for password-based authentication schemes such as basic, digest, NTLM, and Kerberos authentication. */
@:native("System.Net.NetworkCredential")
extern class NetworkCredential {
	/**
	 * Gets or sets the domain or computer name that verifies the credentials.
	 * @return The name of the domain associated with the credentials.
	 */
	var Domain(default, default):String;
	/**
	 * Gets or sets the password for the user name associated with the credentials.
	 * @return The password associated with the credentials. If this  instance was
	 * initialized with the  parameter set to , then the  property will return an empty
	 * string.
	 */
	var Password(default, default):String;
	/**
	 * Gets or sets the password as a  instance.
	 * @return The password for the user name associated with the credentials.
	 */
	var SecurePassword(default, default):cs.system.security.SecureString;
	/**
	 * Gets or sets the user name associated with the credentials.
	 * @return The user name associated with the credentials.
	 */
	var UserName(default, default):String;
	@:overload(function():Void {})
	@:overload(function(userName:String, password:cs.system.security.SecureString):Void {})
	@:overload(function(userName:String, password:String):Void {})
	@:overload(function(userName:String, password:cs.system.security.SecureString, domain:String):Void {})
	function new(userName:String, password:String, domain:String):Void;
	@:overload(function(uri:cs.system.Uri, authType:String):cs.system.net.NetworkCredential {})
	/**
	 * Returns an instance of the  class for the specified host, port, and
	 * authentication type.
	 * @param host The host computer that authenticates the client.
	 * @param port The port on the  that the client communicates with.
	 * @param authenticationType The type of authentication requested, as defined in
	 * the  property.
	 * @return A  for the specified host, port, and authentication protocol, or  if
	 * there are no credentials available for the specified host, port, and
	 * authentication protocol.
	 */
	function GetCredential(host:String, port:Int, authenticationType:String):cs.system.net.NetworkCredential;
}
