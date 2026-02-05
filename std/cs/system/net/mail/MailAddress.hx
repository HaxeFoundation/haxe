package cs.system.net.mail;

/** Represents the address of an electronic mail sender or recipient. */
@:native("System.Net.Mail.MailAddress")
extern class MailAddress {
	/**
	 * Gets the email address specified when this instance was created.
	 * @return A  that contains the email address.
	 */
	var Address(default, never):String;
	/**
	 * Gets the display name composed from the display name and address information
	 * specified when this instance was created.
	 * @return A  that contains the display name; otherwise,  ("") if no display name
	 * information was specified when this instance was created.
	 */
	var DisplayName(default, never):String;
	/**
	 * Gets the host portion of the address specified when this instance was created.
	 * @return A  that contains the name of the host computer that accepts email for
	 * the  property.
	 */
	var Host(default, never):String;
	/**
	 * Gets the user information from the address specified when this instance was
	 * created.
	 * @return A  that contains the user name portion of the .
	 */
	var User(default, never):String;
	@:overload(function(address:String):Void {})
	@:overload(function(address:String, displayName:String):Void {})
	function new(address:String, displayName:String, displayNameEncoding:cs.system.text.Encoding):Void;
	/**
	 * Compares two mail addresses.
	 * @param value A  instance to compare to the current instance.
	 * @return if the two mail addresses are equal; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns a hash value for a mail address.
	 * @return An integer hash value.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string representation of this instance.
	 * @return A  that contains the contents of this .
	 */
	function ToString():String;
}
