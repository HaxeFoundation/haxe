package cs.system.net.mail;

/** The delivery format to use for sending outgoing email using the Simple Mail Transport Protocol (SMTP). */
@:native("System.Net.Mail.SmtpDeliveryFormat")
extern enum SmtpDeliveryFormat {
	International;
	SevenBit;
}
