package cs.system.net;

/** Specifies the status codes returned for a File Transfer Protocol (FTP) operation. */
@:native("System.Net.FtpStatusCode")
extern enum FtpStatusCode {
	AccountNeeded;
	ActionAbortedLocalProcessingError;
	ActionAbortedUnknownPageType;
	ActionNotTakenFilenameNotAllowed;
	ActionNotTakenFileUnavailable;
	ActionNotTakenFileUnavailableOrBusy;
	ActionNotTakenInsufficientSpace;
	ArgumentSyntaxError;
	BadCommandSequence;
	CantOpenData;
	ClosingControl;
	ClosingData;
	CommandExtraneous;
	CommandNotImplemented;
	CommandOK;
	CommandSyntaxError;
	ConnectionClosed;
	DataAlreadyOpen;
	DirectoryStatus;
	EnteringPassive;
	FileActionAborted;
	FileActionOK;
	FileCommandPending;
	FileStatus;
	LoggedInProceed;
	NeedLoginAccount;
	NotLoggedIn;
	OpeningData;
	PathnameCreated;
	RestartMarker;
	SendPasswordCommand;
	SendUserCommand;
	ServerWantsSecureSession;
	ServiceNotAvailable;
	ServiceTemporarilyNotAvailable;
	SystemType;
	Undefined;
}
