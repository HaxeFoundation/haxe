package cs.system.net.mail;

/** Represents the exception that is thrown when the  is not able to complete a  or  operation to a particular recipient. */
@:native("System.Net.Mail.SmtpFailedRecipientException")
extern class SmtpFailedRecipientException extends cs.system.net.mail.SmtpException {
	/**
	 * Indicates the email address with delivery difficulties.
	 * @return A  that contains the email address.
	 */
	var FailedRecipient(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(statusCode:cs.system.net.mail.SmtpStatusCode, failedRecipient:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	@:overload(function(statusCode:cs.system.net.mail.SmtpStatusCode, failedRecipient:String, serverResponse:String):Void {})
	function new(message:String, failedRecipient:String, innerException:cs.system.Exception):Void;
	/**
	 * Populates a  instance with the data that is needed to serialize the .
	 * @param serializationInfo The  to populate with data.
	 * @param streamingContext A  that specifies the destination for this
	 * serialization.
	 */
	function GetObjectData(serializationInfo:cs.system.runtime.serialization.SerializationInfo, streamingContext:cs.system.runtime.serialization.StreamingContext):Void;
}
