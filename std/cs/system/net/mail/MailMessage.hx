package cs.system.net.mail;

/** Represents an email message that can be sent using the  class. */
@:native("System.Net.Mail.MailMessage")
extern class MailMessage {
	/**
	 * Gets the attachment collection used to store alternate forms of the message
	 * body.
	 * @return A writable .
	 */
	var AlternateViews(default, never):cs.system.net.mail.AlternateViewCollection;
	/**
	 * Gets the attachment collection used to store data attached to this email
	 * message.
	 * @return A writable .
	 */
	var Attachments(default, never):cs.system.net.mail.AttachmentCollection;
	/**
	 * Gets the address collection that contains the blind carbon copy (BCC) recipients
	 * for this email message.
	 * @return A writable  object.
	 */
	var Bcc(default, never):cs.system.net.mail.MailAddressCollection;
	/**
	 * Gets or sets the message body.
	 * @return A  value that contains the body text.
	 */
	var Body(default, default):String;
	/**
	 * Gets or sets the encoding used to encode the message body.
	 * @return An  applied to the contents of the .
	 */
	var BodyEncoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets or sets the transfer encoding used to encode the message body.
	 * @return A  applied to the contents of the .
	 */
	var BodyTransferEncoding(default, default):cs.system.net.mime.TransferEncoding;
	/**
	 * Gets the address collection that contains the carbon copy (CC) recipients for
	 * this email message.
	 * @return A writable  object.
	 */
	var CC(default, never):cs.system.net.mail.MailAddressCollection;
	/**
	 * Gets or sets the delivery notifications for this email message.
	 * @return A  value that contains the delivery notifications for this message.
	 */
	var DeliveryNotificationOptions(default, default):cs.system.net.mail.DeliveryNotificationOptions;
	/**
	 * Gets or sets the from address for this email message.
	 * @return A  that contains the from address information.
	 */
	var From(default, default):cs.system.net.mail.MailAddress;
	/**
	 * Gets the email headers that are transmitted with this email message.
	 * @return A  that contains the email headers.
	 */
	var Headers(default, never):cs.system.collections.specialized.NameValueCollection;
	/**
	 * Gets or sets the encoding used for the user-defined custom headers for this
	 * email message.
	 * @return The encoding used for user-defined custom headers for this email
	 * message.
	 */
	var HeadersEncoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets or sets a value indicating whether the mail message body is in HTML.
	 * @return if the message body is in HTML; else . The default is .
	 */
	var IsBodyHtml(default, default):Bool;
	/**
	 * Gets or sets the priority of this email message.
	 * @return A  that contains the priority of this message.
	 */
	var Priority(default, default):cs.system.net.mail.MailPriority;
	/**
	 * Gets or sets the ReplyTo address for the mail message.
	 * @return A MailAddress that indicates the value of the  field.
	 */
	var ReplyTo(default, default):cs.system.net.mail.MailAddress;
	/**
	 * Gets the list of addresses to reply to for the mail message.
	 * @return The list of the addresses to reply to for the mail message.
	 */
	var ReplyToList(default, never):cs.system.net.mail.MailAddressCollection;
	/**
	 * Gets or sets the sender's address for this email message.
	 * @return A  that contains the sender's address information.
	 */
	var Sender(default, default):cs.system.net.mail.MailAddress;
	/**
	 * Gets or sets the subject line for this email message.
	 * @return A  that contains the subject content.
	 */
	var Subject(default, default):String;
	/**
	 * Gets or sets the encoding used for the subject content for this email message.
	 * @return An  that was used to encode the  property.
	 */
	var SubjectEncoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets the address collection that contains the recipients of this email message.
	 * @return A writable  object.
	 */
	var To(default, never):cs.system.net.mail.MailAddressCollection;
	@:overload(function():Void {})
	@:overload(function(from:cs.system.net.mail.MailAddress, to:cs.system.net.mail.MailAddress):Void {})
	@:overload(function(from:String, to:String):Void {})
	function new(from:String, to:String, subject:String, body:String):Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
}
