package cs.system.net.mail;

/** The exception that is thrown when email is sent using an  and cannot be delivered to all recipients. */
@:native("System.Net.Mail.SmtpFailedRecipientsException")
extern class SmtpFailedRecipientsException extends cs.system.net.mail.SmtpFailedRecipientException {
	/**
	 * Gets one or more s that indicate the email recipients with SMTP delivery errors.
	 * @return An array of type  that lists the recipients with delivery errors.
	 */
	var InnerExceptions(default, never):cs.NativeArray<cs.system.net.mail.SmtpFailedRecipientException>;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, innerExceptions:cs.NativeArray<cs.system.net.mail.SmtpFailedRecipientException>):Void;
	/**
	 * Populates a  instance with the data that is needed to serialize the .
	 * @param serializationInfo The  to be used.
	 * @param streamingContext The  to be used.
	 */
	function GetObjectData(serializationInfo:cs.system.runtime.serialization.SerializationInfo, streamingContext:cs.system.runtime.serialization.StreamingContext):Void;
}
