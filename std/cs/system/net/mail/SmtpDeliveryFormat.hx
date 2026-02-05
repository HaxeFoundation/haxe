package cs.system.net.mail;

/** The delivery format to use for sending outgoing email using the Simple Mail Transport Protocol (SMTP). */
@:native("System.Net.Mail.SmtpDeliveryFormat")
extern enum abstract SmtpDeliveryFormat(Int) {
	var International = 1;
	var SevenBit = 0;
}
