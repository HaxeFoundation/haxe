package cs.system.net;

/** The HTTP headers that can be specified in a server response. */
@:native("System.Net.HttpResponseHeader")
extern enum HttpResponseHeader {
	AcceptRanges;
	Age;
	Allow;
	CacheControl;
	Connection;
	ContentEncoding;
	ContentLanguage;
	ContentLength;
	ContentLocation;
	ContentMd5;
	ContentRange;
	ContentType;
	Date;
	ETag;
	Expires;
	KeepAlive;
	LastModified;
	Location;
	Pragma;
	ProxyAuthenticate;
	RetryAfter;
	Server;
	SetCookie;
	Trailer;
	TransferEncoding;
	Upgrade;
	Vary;
	Via;
	Warning;
	WwwAuthenticate;
}
