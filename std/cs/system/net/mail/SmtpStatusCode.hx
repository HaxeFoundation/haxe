package cs.system.net.mail;

/** Specifies the outcome of sending email by using the  class. */
@:native("System.Net.Mail.SmtpStatusCode")
extern enum SmtpStatusCode {
	BadCommandSequence;
	CannotVerifyUserWillAttemptDelivery;
	ClientNotPermitted;
	CommandNotImplemented;
	CommandParameterNotImplemented;
	CommandUnrecognized;
	ExceededStorageAllocation;
	GeneralFailure;
	HelpMessage;
	InsufficientStorage;
	LocalErrorInProcessing;
	MailboxBusy;
	MailboxNameNotAllowed;
	MailboxUnavailable;
	MustIssueStartTlsFirst;
	Ok;
	ServiceClosingTransmissionChannel;
	ServiceNotAvailable;
	ServiceReady;
	StartMailInput;
	SyntaxError;
	SystemStatus;
	TransactionFailed;
	UserNotLocalTryAlternatePath;
	UserNotLocalWillForward;
}
