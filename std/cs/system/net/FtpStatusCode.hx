package cs.system.net;

/** Specifies the status codes returned for a File Transfer Protocol (FTP) operation. */
@:native("System.Net.FtpStatusCode")
extern enum abstract FtpStatusCode(Int) {
	var AccountNeeded = 532;
	var ActionAbortedLocalProcessingError = 451;
	var ActionAbortedUnknownPageType = 551;
	var ActionNotTakenFilenameNotAllowed = 553;
	var ActionNotTakenFileUnavailable = 550;
	var ActionNotTakenFileUnavailableOrBusy = 450;
	var ActionNotTakenInsufficientSpace = 452;
	var ArgumentSyntaxError = 501;
	var BadCommandSequence = 503;
	var CantOpenData = 425;
	var ClosingControl = 221;
	var ClosingData = 226;
	var CommandExtraneous = 202;
	var CommandNotImplemented = 502;
	var CommandOK = 200;
	var CommandSyntaxError = 500;
	var ConnectionClosed = 426;
	var DataAlreadyOpen = 125;
	var DirectoryStatus = 212;
	var EnteringPassive = 227;
	var FileActionAborted = 552;
	var FileActionOK = 250;
	var FileCommandPending = 350;
	var FileStatus = 213;
	var LoggedInProceed = 230;
	var NeedLoginAccount = 332;
	var NotLoggedIn = 530;
	var OpeningData = 150;
	var PathnameCreated = 257;
	var RestartMarker = 110;
	var SendPasswordCommand = 331;
	var SendUserCommand = 220;
	var ServerWantsSecureSession = 234;
	var ServiceNotAvailable = 421;
	var ServiceTemporarilyNotAvailable = 120;
	var SystemType = 215;
	var Undefined = 0;
}
