package cs.system.net;

/** The HTTP headers that may be specified in a client request. */
@:native("System.Net.HttpRequestHeader")
extern enum abstract HttpRequestHeader(Int) {
	var Accept = 20;
	var AcceptCharset = 21;
	var AcceptEncoding = 22;
	var AcceptLanguage = 23;
	var Allow = 10;
	var Authorization = 24;
	var CacheControl = 0;
	var Connection = 1;
	var ContentEncoding = 13;
	var ContentLanguage = 14;
	var ContentLength = 11;
	var ContentLocation = 15;
	var ContentMd5 = 16;
	var ContentRange = 17;
	var ContentType = 12;
	var Cookie = 25;
	var Date = 2;
	var Expect = 26;
	var Expires = 18;
	var From = 27;
	var Host = 28;
	var IfMatch = 29;
	var IfModifiedSince = 30;
	var IfNoneMatch = 31;
	var IfRange = 32;
	var IfUnmodifiedSince = 33;
	var KeepAlive = 3;
	var LastModified = 19;
	var MaxForwards = 34;
	var Pragma = 4;
	var ProxyAuthorization = 35;
	var Range = 37;
	var Referer = 36;
	var Te = 38;
	var Trailer = 5;
	var TransferEncoding = 6;
	var Translate = 39;
	var Upgrade = 7;
	var UserAgent = 40;
	var Via = 8;
	var Warning = 9;
}
