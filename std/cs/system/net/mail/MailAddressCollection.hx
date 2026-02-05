package cs.system.net.mail;

/** Store email addresses that are associated with an email message. */
@:native("System.Net.Mail.MailAddressCollection")
extern class MailAddressCollection extends cs.system.collections.objectmodel.Collection<cs.system.net.mail.MailAddress> {
	function new():Void;
	/**
	 * Add a list of email addresses to the collection.
	 * @param addresses The email addresses to add to the . Multiple email addresses
	 * must be separated with a comma character (",").
	 */
	function Add(addresses:String):Void;
	/**
	 * Returns a string representation of the email addresses in this  object.
	 * @return A  containing the email addresses in this collection.
	 */
	function ToString():String;
}
