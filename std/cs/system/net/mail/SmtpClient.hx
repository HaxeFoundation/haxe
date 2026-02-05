package cs.system.net.mail;

/** Allows applications to send email by using the Simple Mail Transfer Protocol (SMTP). The SmtpClient type is now obsolete. */
@:native("System.Net.Mail.SmtpClient")
extern class SmtpClient {
	/**
	 * Specify which certificates should be used to establish the Secure Sockets Layer
	 * (SSL) connection.
	 * @return An , holding one or more client certificates. The default value is
	 * derived from the mail configuration attributes in a configuration file.
	 */
	var ClientCertificates(default, never):cs.system.security.cryptography.x509certificates.X509CertificateCollection;
	/**
	 * Gets or sets the credentials used to authenticate the sender.
	 * @return An  that represents the credentials to use for authentication; or  if no
	 * credentials have been specified.
	 */
	var Credentials(default, default):cs.system.net.ICredentialsByHost;
	/**
	 * Gets or sets the delivery format used by  to send email.
	 * @return The delivery format used by .
	 */
	var DeliveryFormat(default, default):cs.system.net.mail.SmtpDeliveryFormat;
	/**
	 * Specifies how outgoing email messages will be handled.
	 * @return An  that indicates how email messages are delivered.
	 */
	var DeliveryMethod(default, default):cs.system.net.mail.SmtpDeliveryMethod;
	/**
	 * Specify whether the  uses Secure Sockets Layer (SSL) to encrypt the connection.
	 * @return if the  uses SSL; otherwise, . The default is .
	 */
	var EnableSsl(default, default):Bool;
	/**
	 * Gets or sets the name or IP address of the host used for SMTP transactions.
	 * @return A  that contains the name or IP address of the computer to use for SMTP
	 * transactions.
	 */
	var Host(default, default):String;
	/**
	 * Gets or sets the folder where applications save mail messages to be processed by
	 * the local SMTP server.
	 * @return A  that specifies the pickup directory for mail messages.
	 */
	var PickupDirectoryLocation(default, default):String;
	/**
	 * Gets or sets the port used for SMTP transactions.
	 * @return An  that contains the port number on the SMTP host. The default value is
	 * 25.
	 */
	var Port(default, default):Int;
	/**
	 * Gets the network connection used to transmit the email message.
	 * @return A  that connects to the  property used for SMTP.
	 */
	var ServicePoint(default, never):cs.system.net.ServicePoint;
	/**
	 * Gets or sets the Service Provider Name (SPN) to use for authentication when
	 * using extended protection.
	 * @return A  that specifies the SPN to use for extended protection. The default
	 * value for this SPN is of the form "SMTPSVC/<host>" where <host> is the hostname
	 * of the SMTP mail server.
	 */
	var TargetName(default, default):String;
	/**
	 * Gets or sets a value that specifies the amount of time after which a synchronous
	 * call times out.
	 * @return An  that specifies the time-out value in milliseconds. The default value
	 * is 100,000 (100 seconds).
	 */
	var Timeout(default, default):Int;
	/**
	 * Gets or sets a  value that controls whether the  are sent with requests.
	 * @return if the default credentials are used; otherwise . The default value is .
	 */
	var UseDefaultCredentials(default, default):Bool;
	@:overload(function():Void {})
	@:overload(function(host:String):Void {})
	function new(host:String, port:Int):Void;
	/** Sends a QUIT message to the SMTP server, gracefully ends the TCP connection, and releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	@:overload(function(message:cs.system.net.mail.MailMessage):Void {})
	/**
	 * Sends the specified message to an SMTP server for delivery.
	 * @param message A  that contains the message to send.
	 */
	function Send(from:String, recipients:String, subject:String, body:String):Void;
	@:overload(function(message:cs.system.net.mail.MailMessage, userToken:Dynamic):Void {})
	/**
	 * Sends the specified email message to an SMTP server for delivery. This method
	 * does not block the calling thread and allows the caller to pass an object to the
	 * method that is invoked when the operation completes.
	 * @param message A  that contains the message to send.
	 * @param userToken A user-defined object that is passed to the method invoked when
	 * the asynchronous operation completes.
	 */
	function SendAsync(from:String, recipients:String, subject:String, body:String, userToken:Dynamic):Void;
	/** Cancels an asynchronous operation to send an email message. */
	function SendAsyncCancel():Void;
	@:overload(function(message:cs.system.net.mail.MailMessage):cs.system.threading.tasks.Task {})
	/**
	 * Sends the specified message to an SMTP server for delivery as an asynchronous
	 * operation.
	 * @param message A  that contains the message to send.
	 * @return The task object representing the asynchronous operation.
	 */
	function SendMailAsync(from:String, recipients:String, subject:String, body:String):cs.system.threading.tasks.Task;
}
