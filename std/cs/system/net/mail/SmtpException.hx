package cs.system.net.mail;

/** Represents the exception that is thrown when the  is not able to complete a  or  operation. */
@:native("System.Net.Mail.SmtpException")
extern class SmtpException extends cs.system.Exception {
	/**
	 * Gets the status code returned by an SMTP server when an email message is
	 * transmitted.
	 * @return An  value that indicates the error that occurred.
	 */
	var StatusCode(default, default):cs.system.net.mail.SmtpStatusCode;
	@:overload(function():Void {})
	@:overload(function(statusCode:cs.system.net.mail.SmtpStatusCode):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(statusCode:cs.system.net.mail.SmtpStatusCode, message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
	/**
	 * Populates a  instance with the data needed to serialize the .
	 * @param serializationInfo The  to populate with data.
	 * @param streamingContext A  that specifies the destination for this
	 * serialization.
	 */
	function GetObjectData(serializationInfo:cs.system.runtime.serialization.SerializationInfo, streamingContext:cs.system.runtime.serialization.StreamingContext):Void;
}
