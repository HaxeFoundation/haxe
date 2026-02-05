package cs.system.net.mail;

/** Specifies the outcome of sending email by using the  class. */
@:native("System.Net.Mail.SmtpStatusCode")
extern enum abstract SmtpStatusCode(Int) {
	var BadCommandSequence = 503;
	var CannotVerifyUserWillAttemptDelivery = 252;
	var ClientNotPermitted = 454;
	var CommandNotImplemented = 502;
	var CommandParameterNotImplemented = 504;
	var CommandUnrecognized = 500;
	var ExceededStorageAllocation = 552;
	var GeneralFailure = -1;
	var HelpMessage = 214;
	var InsufficientStorage = 452;
	var LocalErrorInProcessing = 451;
	var MailboxBusy = 450;
	var MailboxNameNotAllowed = 553;
	var MailboxUnavailable = 550;
	var MustIssueStartTlsFirst = 530;
	var Ok = 250;
	var ServiceClosingTransmissionChannel = 221;
	var ServiceNotAvailable = 421;
	var ServiceReady = 220;
	var StartMailInput = 354;
	var SyntaxError = 501;
	var SystemStatus = 211;
	var TransactionFailed = 554;
	var UserNotLocalTryAlternatePath = 551;
	var UserNotLocalWillForward = 251;
}
