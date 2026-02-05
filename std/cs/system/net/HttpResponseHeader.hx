package cs.system.net;

/** The HTTP headers that can be specified in a server response. */
@:native("System.Net.HttpResponseHeader")
extern enum abstract HttpResponseHeader(Int) {
	var AcceptRanges = 20;
	var Age = 21;
	var Allow = 10;
	var CacheControl = 0;
	var Connection = 1;
	var ContentEncoding = 13;
	var ContentLanguage = 14;
	var ContentLength = 11;
	var ContentLocation = 15;
	var ContentMd5 = 16;
	var ContentRange = 17;
	var ContentType = 12;
	var Date = 2;
	var ETag = 22;
	var Expires = 18;
	var KeepAlive = 3;
	var LastModified = 19;
	var Location = 23;
	var Pragma = 4;
	var ProxyAuthenticate = 24;
	var RetryAfter = 25;
	var Server = 26;
	var SetCookie = 27;
	var Trailer = 5;
	var TransferEncoding = 6;
	var Upgrade = 7;
	var Vary = 28;
	var Via = 8;
	var Warning = 9;
	var WwwAuthenticate = 29;
}
