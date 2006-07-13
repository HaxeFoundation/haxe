package mtwin.mail;

enum Exception {
	ConnectionError(host:String,port:Int);
	SmtpMailFromError(e:String);
	SmtpRcptToError(e:String);
	SmtpDataError(e:String);
	SmtpSendDataError;

	UnknowResponse(r:String);
	BadResponse(r:String);
	ImapFetchError(id:Int);
}
