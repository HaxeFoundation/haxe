package cs.system.net;

/** The HTTP headers that may be specified in a client request. */
@:native("System.Net.HttpRequestHeader")
extern enum HttpRequestHeader {
	Accept;
	AcceptCharset;
	AcceptEncoding;
	AcceptLanguage;
	Allow;
	Authorization;
	CacheControl;
	Connection;
	ContentEncoding;
	ContentLanguage;
	ContentLength;
	ContentLocation;
	ContentMd5;
	ContentRange;
	ContentType;
	Cookie;
	Date;
	Expect;
	Expires;
	From;
	Host;
	IfMatch;
	IfModifiedSince;
	IfNoneMatch;
	IfRange;
	IfUnmodifiedSince;
	KeepAlive;
	LastModified;
	MaxForwards;
	Pragma;
	ProxyAuthorization;
	Range;
	Referer;
	Te;
	Trailer;
	TransferEncoding;
	Translate;
	Upgrade;
	UserAgent;
	Via;
	Warning;
}
