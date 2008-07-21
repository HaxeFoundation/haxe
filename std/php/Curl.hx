package php;
/**
* TODO: TEST IT!
*/
class Curl {
	var h : Void;
	public inline function new(?url : String) {
		if(url != null)
			h = untyped __call__("curl_init", url);
		else
			h = untyped __call__("curl_init");
	}
	
	public inline function close() {
		untyped __call__("curl_close", h);
	}
	
	public function clone() {
		var n = new Curl();
		untyped n.h = untyped __call__("curl_copy_handle", h);
		return n;
	}
	
	/**
	* CURLE_OK = 0,
	* CURLE_UNSUPPORTED_PROTOCOL,    1
	* CURLE_FAILED_INIT,             2
	* CURLE_URL_MALFORMAT,           3
	* CURLE_URL_MALFORMAT_USER,      4 - NOT USED
	* CURLE_COULDNT_RESOLVE_PROXY,   5
	* CURLE_COULDNT_RESOLVE_HOST,    6
	* CURLE_COULDNT_CONNECT,         7
	* CURLE_FTP_WEIRD_SERVER_REPLY,  8
	*/	
	public inline function errNo() : Int {
		return untyped __call__("curl_errno", h);
	}
	
	public inline function error() : String {
		return untyped __call__("curl_error", h);
	}
	
	public inline function execute() : Bool {
		return untyped __call__("curl_exec", h);
	}
	
	public inline function executeTransfer() : String {
		setReturnTransfer(true);
		var r : String = untyped __call__("curl_exec", h);
		if(untyped r == false) 
			return null;
		else
			return r;
	}
	
	public inline function version(?age : Int) : {
		version_number : Int,
		version : String,
		ssl_version_number : Int,
		ssl_version : String,
		libz_version : String,
		host : String,
		age : Int,
		features : Int,
		protocols : Array<String> } {
		var v = untyped __call__("curl_version", age);
		return php.Boot.__anonymous(v);
	}
	/**
	* Last effective URL
	*/
	public inline function getEffectiveUrl() : String {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_EFFECTIVE_URL"));
	}
	
	/**
	* Last received HTTP code
	*/
	// TODO: check return time is really Int
	public inline function getHttpCode() : Int {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_HTTP_CODE"));
	}
	
	/**
	* Remote time of the retrieved document, if -1 is returned the time of the document is unknown
	*/
	public inline function getFileTime() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_FILETIME"));
	}
	
	/**
	* Total transaction time in seconds for last transfer
	*/
	public inline function getTotalTime() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_TOTAL_TIME"));
	}
	
	/**
	* Time in seconds until name resolving was complete
	*/
	public inline function getNameLookupTime() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_NAMELOOKUP_TIME"));
	}
	
	/**
	* Time in seconds it took to establish the connection
	*/
	public inline function getConnectTime() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_CONNECT_TIME"));
	}
	
	/**
	* Time in seconds from start until just before file transfer begins
	*/
	public inline function getPreTransferTime() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_PRETRANSFER_TIME"));
	}
	
	/**
	* Time in seconds until the first byte is about to be transferred
	*/
	public inline function getStartTransferTime() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_STARTTRANSFER_TIME"));
	}
	
	/**
	* Time in seconds of all redirection steps before final transaction was started
	*/
	public inline function getRedirectTime() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_REDIRECT_TIME"));
	}
	
	/**
	* Total number of bytes uploaded
	*/
	public inline function getSizeUpload() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_SIZE_UPLOAD"));
	}
	
	/**
	* Total number of bytes downloaded
	*/
	public inline function getSizeDownload() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_SIZE_DOWNLOAD"));
	}
	
	/**
	* Average download speed
	*/
	public inline function getSpeedDownload() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_SPEED_DOWNLOAD"));
	}
	
	/**
	* Average upload speed
	*/
	public inline function getSpeedUpload() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_SPEED_UPLOAD"));
	}
	
	/**
	* Total size of all headers received
	*/
	// TODO: maybe this can return very big values so a Float is a better option
	public inline function getHeaderSize() : Int {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_HEADER_SIZE"));
	}
	
	/**
	* The request string sent. Available since PHP 5.1.3
	*/
	public inline function getHeaderOut() : String {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_HEADER_OUT"));
	}
	
	/**
	* Total size of issued requests, currently only for HTTP requests
	*/
	public inline function getRequestSize() : Int {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_REQUEST_SIZE"));
	}
	
	/**
	* Result of SSL certification verification requested by setting setSsslVerifyPeer
	*/
	// TODO: is the return type correct?
	public inline function getSslVerifyResult() : Bool {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_SSL_VERIFYRESULT"));
	}
	
	/**
	* content-length of download, read from Content-Length: field
	*/
	public inline function getContentLengthDownload() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_CONTENT_LENGTH_DOWNLOAD"));
	}
	
	/**
	* Specified size of upload
	*/
	public inline function getContentLengthUpload() : Float {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_CONTENT_LENGTH_UPLOAD"));
	}
	
	/**
	* Content-type of downloaded object, NULL indicates server did not send valid Content-Type: header
	*/
	public inline function getContentType() : String {
		return untyped __call__("curl_getinfo", h, __php__("CURLINFO_CONTENT_TYPE"));
	}
	
	/**
	* TRUE to automatically set the Referer: field in requests where it 
	* follows a Location: redirect.
	* Available since PHP 5.1.0.
	*/
	public inline function setAutoReferer(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_AUTOREFERER"), v);
	}
	
	/**
	* TRUE to return the raw output when setReturnTransfer is used. 	
	*/
	public inline function setBinaryTransfer(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_BINARYTRANSFER"), v);
	}

	/**
	* TRUE to mark this as a new cookie "session". It will force libcurl 
	* to ignore all cookies it is about to load that are "session cookies"
	* from the previous session. By default, libcurl always stores and
	* loads all cookies, independent if they are session cookies are not.
	* Session cookies are cookies without expiry date and they are meant 
	* to be alive and existing for this "session" only.
	* Available since PHP 5.1.0.
	*/
	public inline function setCookieSession(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_COOKIESESSION"), v);
	}
	
	/**
	* TRUE to convert Unix newlines to CRLF newlines on transfers.
	*/
	public inline function setCrLf(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_CRLF"), v);
	}
	
	/**
	* TRUE to use a global DNS cache. This option is not thread-safe and is enabled by default. 	
	*/
	public inline function setDnsUseGlobalCache(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_DNS_USE_GLOBAL_CACHE"), v);
	}
	
	/**
	* TRUE to fail silently if the HTTP code returned is greater than or equal to 400. The default behavior is to return the page normally, ignoring the code. 	
	*/
	public inline function setFailOnError(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FAILONERROR"), v);
	}
	
	/**
	* TRUE to attempt to retrieve the modification date of the remote document. This value can be retrieved using the CURLINFO_FILETIME option with curl_getinfo(). 	
	*/
	public inline function setFileTime(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FILETIME"), v);
	}

	/**
	* TRUE to follow any "Location: " header that the server sends as part of the HTTP header (note this is recursive, PHP will follow as many "Location: " headers that it is sent, unless CURLOPT_MAXREDIRS is set). 	
	*/
	public inline function setFollowLocation(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FOLLOWLOCATION"), v);
	}
	
	/**
	* TRUE to force the connection to explicitly close when it has finished processing, and not be pooled for reuse. 	
	*/
	public inline function setForbidReuse(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FORBID_REUSE"), v);
	}
	
	/**
	* TRUE to force the use of a new connection instead of a cached one. 	
	*/
	public inline function setFreschConnect(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FRESH_CONNECT"), v);
	}
	
	/**
	* TRUE to use EPRT (and LPRT) when doing active FTP downloads. Use FALSE to disable EPRT and LPRT and use PORT only. 	Added in PHP 5.0.0.
	*/
	public inline function setFtpUseEprt(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FTP_USE_EPRT"), v);
	}
	
	/**
	* TRUE to first try an EPSV command for FTP transfers before reverting back to PASV. Set to FALSE to disable EPSV. 	
	*/
	public inline function setFtpUseEpsv(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FTP_USE_EPSV"), v);
	}

	/**
	* TRUE to append to the remote file instead of overwriting it. 	
	*/
	public inline function setFtpAppend(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FTPAPPEND"), v);
	}
	
	/**
	* TRUE to only list the names of an FTP directory. 	
	*/
	public inline function setFtpListOnly(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FTPLISTONLY"), v);
	}
	
	/**
	* TRUE to include the header in the output. 	
	*/
	public inline function setHeader(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HEADER"), v);
	}
	
	/**
	* TRUE to reset the HTTP request method to GET. Since GET is the default, this is only necessary if the request method has been changed. 	
	*/
	public inline function setHttpGet(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HTTPGET"), v);
	}
	
	/**
	* TRUE to tunnel through a given HTTP proxy. 	
	*/
	public inline function setHttpProxyTunnel(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HTTPPROXYTUNNEL"), v);
	}
	
	/**
	* TRUE to be completely silent with regards to the cURL functions. 	
	*/
	public inline function setMute(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_MUTE"), v);
	}
	
	/**
	* TRUE to scan the ~/.netrc file to find a username and password for the remote site that a connection is being established with. 	
	*/
	public inline function setNetRc(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_NETRC"), v);
	}
	
	/**
	* TRUE to exclude the body from the output. 	
	*/
	public inline function setNoBody(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_NOBODY"), v);
	}
	
	/**
	* TRUE to disable the progress meter for cURL transfers. Note: PHP automatically sets this option to TRUE, this should only be changed for debugging purposes.
	*/
	public inline function setNoProgress(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_NOPROGRESS"), v);
	}
 	
	/**
	* TRUE to ignore any cURL function that causes a signal to be sent to the PHP process. This is turned on by default in multi-threaded SAPIs so timeout options can still be used. 	Added in cURL 7.10 and PHP 5.0.0.
	*/
	public inline function setNoSignal(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_NOSIGNAL"), v);
	}
	
	/**
	* TRUE to do a regular HTTP POST. This POST is the normal application/x-www-form-urlencoded kind, most commonly used by HTML forms. 	
	*/
	public inline function setPost(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_POST"), v);
	}
	
	/**
	* TRUE to HTTP PUT a file. The file to PUT must be set with CURLOPT_INFILE and CURLOPT_INFILESIZE. 	
	*/
	public inline function setPut(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_PUT"), v);
	}
	
	/**
	* TRUE to return the transfer as a string of the return value of curl_exec() instead of outputting it out directly. 	
	*/
	public inline function setReturnTransfer(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_RETURNTRANSFER"), v);
	}
	
	/**
	* FALSE to stop cURL from verifying the peer's certificate. Alternate certificates to verify against can be specified with the CURLOPT_CAINFO option or a certificate directory can be specified with the CURLOPT_CAPATH option. CURLOPT_SSL_VERIFYHOST may also need to be TRUE or FALSE if CURLOPT_SSL_VERIFYPEER is disabled (it defaults to 2). 	TRUE by default as of cURL 7.10. Default bundle installed as of cURL 7.10.
	*/
	public inline function setSslVerifyPeer(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSL_VERIFYPEER"), v);
	}

 	/**
	* TRUE to use ASCII mode for FTP transfers. For LDAP, it retrieves data in plain text instead of HTML. On Windows systems, it will not set STDOUT to binary mode. 	
	*/
	public inline function setTransferText(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_TRANSFERTEXT"), v);
	}
	
	/**
	* TRUE to keep sending the username and password when following locations (using CURLOPT_FOLLOWLOCATION), even when the hostname has changed. 	Added in PHP 5.0.0.
	*/
	public inline function setUnrestrictedAuth(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_UNRESTRICTED_AUTH"), v);
	}
	
	/**
	* TRUE to prepare for an upload. 	
	*/
	public inline function setUpload(v : Bool) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_UPLOAD"), v);
	}
	
	/**
	* TRUE to output verbose information. Writes output to STDERR, or the file specified using CURLOPT_STDERR. 	
	*/
	public inline function setVerbose(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_VERBOSE"), v);
	} 

	/**
	* The size of the buffer to use for each read. There is no guarantee this request will be fulfilled, however. 	Added in cURL 7.10 and PHP 5.0.0.
	*/
	public inline function setBufferSize(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_BUFFERSIZE"), v);
	}

	/**
	* Either CURLCLOSEPOLICY_LEAST_RECENTLY_USED or CURLCLOSEPOLICY_OLDEST . There are three other CURLCLOSEPOLICY_ constants, but cURL does not support them yet. 	
	*/
	public inline function setClosePolicy(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_CLOSEPOLICY"), v);
	}
	
	/**
	* The number of seconds to wait whilst trying to connect. Use 0 to wait indefinitely. 	
	*/
	public inline function setConnectTimeout(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_CONNECTTIMEOUT"), v);
	}
	
	/**
	* The number of seconds to keep DNS entries in memory. This option is set to 120 (2 minutes) by default. 	
	*/
	public inline function setDnsCacheTimeout(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_DNS_CACHE_TIMEOUT"), v);
	}
	
	/**
	* The FTP authentication method (when is activated): CURLFTPAUTH_SSL (try SSL first), CURLFTPAUTH_TLS (try TLS first), or CURLFTPAUTH_DEFAULT (let cURL decide). 	Added in cURL 7.12.2 and PHP 5.1.0.
	*/
	public inline function setFtpSslAuth(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FTPSSLAUTH"), v);
	}
	
	/**
	* lets CURL decide which version to use
	*/
	public inline function setHttpDefaultVersion() : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HTTP_VERSION"), __php__("CURL_HTTP_VERSION_NONE"));
	}
	
	/**
	* lets CURL decide which version to use
	*/
	public inline function setHttpVersion10() : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HTTP_VERSION"), __php__("CURL_HTTP_VERSION_1_0"));
	}
	
	/**
	* lets CURL decide which version to use
	*/
	public inline function setHttpVersion11() : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HTTP_VERSION"), __php__("CURL_HTTP_VERSION_1_1"));
	}

	public static var CURLAUTH_BASIC : Int = untyped __php__("CURLAUTH_BASIC");
	public static var CURLAUTH_DIGEST : Int = untyped __php__("CURLAUTH_DIGEST");
	public static var CURLAUTH_GSSNEGOTIATE : Int = untyped __php__("CURLAUTH_GSSNEGOTIATE");
	public static var CURLAUTH_NTLM : Int = untyped __php__("CURLAUTH_NTLM");
	public static var CURLAUTH_ANY : Int = untyped __php__("CURLAUTH_ANY");
	public static var CURLAUTH_ANYSAFE : Int = untyped __php__("CURLAUTH_ANYSAFE");
	
	/**
	* The HTTP authentication method(s) to use. The options are: CURLAUTH_BASIC , CURLAUTH_DIGEST , CURLAUTH_GSSNEGOTIATE , CURLAUTH_NTLM , CURLAUTH_ANY , and CURLAUTH_ANYSAFE . The bitwise | (or) operator can be used to combine more than one method. If this is done, cURL will poll the server to see what methods it supports and pick the best one.
	*/
	public inline function setHttpAuth(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HTTPAUTH"), v);
	}

	/**
	* The expected size, in bytes, of the file when uploading a file to a remote site. 	
	*/
	public inline function setInFileSize(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_INFILESIZE"), v);
	}
	
	/**
	* The transfer speed, in bytes per second, that the transfer should be below during CURLOPT_LOW_SPEED_TIME seconds for PHP to consider the transfer too slow and abort. 	
	*/
	public inline function setLowSpeedLimit(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_LOW_SPEED_LIMIT"), v);
	}

	/**
	* The number of seconds the transfer should be below CURLOPT_LOW_SPEED_LIMIT for PHP to consider the transfer too slow and abort. 	
	*/
	public inline function setLowSpeedTime(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_LOW_SPEED_TIME"), v);
	}
	
	/**
	* The maximum amount of persistent connections that are allowed. When the limit is reached, CURLOPT_CLOSEPOLICY is used to determine which connection to close. 	
	*/
	public inline function setMaxConnects(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_MAXCONNECTS"), v);
	}
	
	/**
	* The maximum amount of HTTP redirections to follow. Use this option alongside CURLOPT_FOLLOWLOCATION. 	
	*/
	public inline function setMaxRedirs(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_MAXREDIRS"), v);
	}
	
	/**
	* An alternative port number to connect to. 	
	*/
	public inline function setPort(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_PORT"), v);
	}
	
	/**
	* The HTTP authentication method(s) to use for the proxy connection. Use the same bitmasks as described in CURLOPT_HTTPAUTH. For proxy authentication, only CURLAUTH_BASIC and CURLAUTH_NTLM are currently supported. 	Added in cURL 7.10.7 and PHP 5.1.0.
	*/
	public inline function setProxyAuth(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_PROXYAUTH"), v);
	}

	/**
	* The port number of the proxy to connect to. This port number can also be set in CURLOPT_PROXY. 	Added in PHP 5.0.0.
	*/
	public inline function setProxyPort(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_PROXYPORT"), v);
	}
	
	/**
	* Either CURLPROXY_HTTP (default) or CURLPROXY_SOCKS5 . 	Added in cURL 7.10 and PHP 5.0.0.
	*/
	public inline function setProxyType(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_PROXYTYPE"), v);
	}
	
	/**
	* The offset, in bytes, to resume a transfer from.
	*/
	public inline function setResumeForm(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_RESUME_FROM"), v);
	}
	
	/**
	* 1 to check the existence of a common name in the SSL peer certificate. 2 to check the existence of a common name and also verify that it matches the hostname provided. 
	*/
	public inline function setSslVerifyHost(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSL_VERIFYHOST"), v);
	}

	/**
	* The SSL version (2 or 3) to use. By default PHP will try to determine this itself, although in some cases this must be set manually. 	
	*/
	public inline function setSslVersion(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSLVERSION"), v);
	}

	/**
	* How setTimeValue() is treated. Use setTimeConditionIsUnmodSince() for the reverse effect of setTimeConditionIfModSince(). Added in PHP 5.1.0.
	*/
	public inline function setTimeConditionIsUnmodSince() : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_TIMECONDITION"), __php__("CURL_TIMECOND_IFMODSINCE"));
	}

	/**
	* How setTimeValue() is treated. Use setTimeConditionIfModSince to return the page only if it has been modified since the time specified in setTimeValue(true). If it hasn't been modified, a "304 Not Modified" header will be returned assuming setHeader(true). 
	* This is the default behavior.
	* Added in PHP 5.1.0.
	*/
	public inline function setTimeConditionIfModSince() : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_TIMECONDITION"), __php__("CURL_TIMECOND_ISUNMODSINCE"));
	}
	
	/**
	* The maximum number of seconds to allow cURL functions to execute. 	
	*/
	public inline function setTimeOut(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_TIMEOUT"), v);
	}
	
	/**
	* The time in seconds since January 1st, 1970. The time will be used by CURLOPT_TIMECONDITION. By default, CURL_TIMECOND_IFMODSINCE is used. 	
	*/
	public inline function setTimeValue(v : Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_TIMEVALUE"), v);
	}
	
	/**
	* The name of a file holding one or more certificates to verify the peer with. This only makes sense when used in combination with CURLOPT_SSL_VERIFYPEER. 	
	*/
	public inline function setCaInfo(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_CAINFO"), v);
	}
	
	/**
	* A directory that holds multiple CA certificates. Use this option alongside CURLOPT_SSL_VERIFYPEER. 	
	*/
	public inline function setCaPath(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_CAPATH"), v);
	}

	/**
	* The contents of the "Set-Cookie: " header to be used in the HTTP request. 	
	*/
	public inline function setCookie(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_COOKIE"), v);
	}
	
	/**
	* The name of the file containing the cookie data. The cookie file can be in Netscape format, or just plain HTTP-style headers dumped into a file. 	
	*/
	public inline function setCookieFile(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_COOKIEFILE"), v);
	}
	
	/**
	* The name of a file to save all internal cookies to when the connection closes. 	
	*/
	public inline function setCookieJar(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_COOKIEJAR"), v);
	}
	
	/**
	* A custom request method to use instead of "GET" or "HEAD" when doing a HTTP request. This is useful for doing "DELETE" or other, more obscure HTTP requests. Valid values are things like "GET", "POST", "CONNECT" and so on; i.e. Do not enter a whole HTTP request line here. For instance, entering "GET /index.html HTTP/1.0\r\n\r\n" would be incorrect.  Note: Don't do this without making sure the server supports the custom request method first.
	*/
	public inline function setCustomRequest(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_CUSTOMREQUEST"), v);
	}
	
	/**
	* Like setRandomFile(), except a filename to an Entropy Gathering Daemon socket. 	
	*/
	public inline function setEgdSocket(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_EGDSOCKET"), v);
	}

	/**
	* The contents of the "Accept-Encoding: " header. This enables decoding of the response. Supported encodings are "identity", "deflate", and "gzip". If an empty string, "", is set, a header containing all supported encoding types is sent. 	Added in cURL 7.10.
	*/
	public inline function setEncoding(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_ENCODING"), v);
	}

	/**
	* The value which will be used to get the IP address to use for the FTP "POST" instruction. The "POST" instruction tells the remote server to connect to our specified IP address. The string may be a plain IP address, a hostname, a network interface name (under Unix), or just a plain '-' to use the systems default IP address. 	
	*/
	public inline function setFtpPort(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FTPPORT"), v);
	}
	
	/**
	* The name of the outgoing network interface to use. This can be an interface name, an IP address or a host name. 	
	*/
	public inline function setInterface(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_INTERFACE"), v);
	}
	
	/**
	* The KRB4 (Kerberos 4) security level. Any of the following values (in order from least to most powerful) are valid: "clear", "safe", "confidential", "private".. If the string does not match one of these, "private" is used. Setting this option to NULL will disable KRB4 security. Currently KRB4 security only works with FTP transactions. 	
	*/
	public inline function setKrb4Level(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_KRB4LEVEL"), v);
	}
	
	/**
	* The full data to post in a HTTP "POST" operation. To post a file, prepend a filename with @ and use the full path. 	
	*/
	public inline function setPostFields(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_POSTFIELDS"), v);
	}

	/**
	* The HTTP proxy to tunnel requests through. 	
	*/
	public inline function setProxy(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_PROXY"), v);
	}
	
	/**
	* A username and password formatted as "[username]:[password]" to use for the connection to the proxy. 	
	*/
	public inline function setProxyUserPwd(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_PROXYUSERPWD"), v);
	}
	
	/**
	* A filename to be used to seed the random number generator for SSL. 	
	*/
	public inline function setRandomFile(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_RANDOM_FILE"), v);
	}
	
	/**
	* Range(s) of data to retrieve in the format "X-Y" where X or Y are optional. HTTP transfers also support several intervals, separated with commas in the format "X-Y,N-M". 	
	*/
	public inline function setRange(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_RANGE"), v);
	}
	
	/**
	* The contents of the "Referer: " header to be used in a HTTP request. 	
	*/
	public inline function setReferer(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_REFERER"), v);
	}
	
	/**
	* A list of ciphers to use for SSL. For example, RC4-SHA and TLSv1 are valid cipher lists. 	
	*/
	public inline function setSslCipherList(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSL_CIPHER_LIST"), v);
	}
	
	/**
	* The name of a file containing a PEM formatted certificate. 	
	*/
	public inline function setSslCert(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSLCERT"), v);
	}
	
	/**
	* The password required to use the CURLOPT_SSLCERT certificate. 	
	*/
	public inline function setSslCertPasswd(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSLCERTPASSWD"), v);
	}
	
	/**
	* The format of the certificate. Supported formats are "PEM" (default), "DER", and "ENG". 	Added in cURL 7.9.3 and PHP 5.0.0.
	*/
	public inline function setSslCertType(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSLCERTTYPE"), v);
	}
	
	/**
	* The identifier for the crypto engine of the private SSL key specified in CURLOPT_SSLKEY. 	
	*/
	public inline function setSslEngine(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSLENGINE"), v);
	}

	/**
	* The identifier for the crypto engine used for asymmetric crypto operations. 	
	*/
	public inline function setSsleEngineDefault(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSLENGINE_DEFAULT"), v);
	}
	
	/**
	* The name of a file containing a private SSL key. 	
	*/
	public inline function setSslKey(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSLKEY"), v);
	}
	
	/**
	* The secret password needed to use the private SSL key specified in CURLOPT_SSLKEY. Note: Since this option contains a sensitive password, remember to keep the PHP script it is contained within safe.
	*/
	public inline function setSslKeyPasswd(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSLKEYPASSWD"), v);
	}
	
	/**
	* The key type of the private SSL key specified in CURLOPT_SSLKEY. Supported key types are "PEM" (default), "DER", and "ENG". 	
	*/
	public inline function setSslKeyType(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_SSLKEYTYPE"), v);
	}
	
	/**
	* The URL to fetch. This can also be set when initializing a session with curl_init(). 	
	*/
	public inline function setUrl(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_URL"), v);
	}

	/**
	* The contents of the "User-Agent: " header to be used in a HTTP request. 	
	*/
	public inline function setUserAgent(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_USERAGENT"), v);
	}
	
	/**
	* A username and password formatted as "[username]:[password]" to use for the connection. 
	*/
	public inline function setUserPwd(v : String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_USERPWD"), v);
	}
	
	/**
	* An array of HTTP 200 responses that will be treated as valid responses and not as errors. 	Added in cURL 7.10.3 and PHP 5.0.0.
	*/
	public inline function setHttp200Aliases(v : Array<Int>) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HTTP200ALIASES"), v);
	}
	
	/**
	* An array of HTTP header fields to set. 	
	*/
	public inline function setHttpHeader(v : Array<String>) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HTTPHEADER"), v);
	}
	
	/**
	* An array of FTP commands to execute on the server after the FTP request has been performed. 	
	*/
	public inline function setPostQuote(v : Array<String>) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_POSTQUOTE"), v);
	}
	
	/**
	* An array of FTP commands to execute on the server prior to the FTP request. 	
	*/
	public inline function setQuote(v : Array<String>) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_QUOTE"), v);
	}
	
	/**
	* The file that the transfer should be written to. The default is STDOUT (the browser window). 	
	*/
	// TODO: adjust the followings to be more haxe oriented
	public inline function setFile(v : Dynamic) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_FILE"), v);
	}
	
	/**
	* The file that the transfer should be read from when uploading. 	
	*/
	public inline function setInFile(v : Dynamic) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_INFILE"), v);
	}
	
	/**
	* An alternative location to output errors to instead of STDERR. 	
	*/
	public inline function setStdErr(v : Dynamic) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_STDERR"), v);
	}
	
	/**
	* The file that the header part of the transfer is written to. 	
	*/
	public inline function setWriteHeader(v : Dynamic) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_WRITEHEADER"), v);
	}	
 	
	/**
	* The name of a callback function where the callback function takes two parameters. The first is the cURL resource, the second is a string with the header data to be written. The header data must be written when using this callback function. Return the number of bytes written. 	
	*/
	public inline function setHeaderFunction(v : Dynamic -> String -> Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_HEADERFUNCTION"), v);
	}
	
	/**
	* The name of a callback function where the callback function takes three parameters. The first is the cURL resource, the second is a string containing a password prompt, and the third is the maximum password length. Return the string containing the password. 	
	*/
	public inline function setPasswdFunction(v : Dynamic -> String -> Int -> String) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_PASSWDFUNCTION"), v);
	}
	
	/**
	* The name of a callback function where the callback function takes two parameters. The first is the cURL resource, and the second is a string with the data to be read. The data must be read by using this callback function. Return the number of bytes read. Return 0 to signal EOF. 	
	*/
	public inline function setReadFunction(v : Dynamic -> String -> Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_READFUNCTION"), v);
	}
	
	/**
	* The name of a callback function where the callback function takes two parameters. The first is the cURL resource, and the second is a string with the data to be written. The data must be written by using this callback function. Must return the exact number of bytes written or this will fail. 
	*/
	public inline function setWriteFunction(v : Dynamic -> String -> Int) : Bool {
		return untyped __call__("curl_setopt", h, __php__("CURLOPT_WRITEFUNCTION"), v);
	}
	
	private static function __init__() {
		untyped __php__('if (!extension_loaded("Curl")) dl("php_curl.".(PHP_OS == "WINNT" || PHP_OS == "WIN32" ? "dll" : "so"))');
	}
}

class CurlMulti {
	var mh : Void;
	public function new() {
		stillRunning = false;
		mh = untyped __call__("curl_multi_init");
	}
	
	public inline function close() {
		untyped __call__("curl_multi_close", mh);
	}
	
	public var stillRunning(default, null) : Bool;
	public inline function execute() : Int {
		var i;
		var r = untyped __call__("curl_multi_exec", mh, i);
		stillRunning = i == 1;
		return r;
	}
	
	public inline function getContent() : String {
		return untyped __call__("curl_multi_getcontent", mh);
	}
	
	public inline function infoRead(?msgsinqueue : Int) : Hash<String> {
		var m = untyped __call__("curl_multi_info_read", mh, msgsinqueue);
		if(m == false)
			return new Hash();
		else
			return Hash.fromAssociativeArray(m);
	}
	
	public function addCurl(c : Curl) : Int {
		return untyped __call__("curl_multi_add_handle", mh, c.h);
	}
	
	public function removeCurl(c : Curl) {
		return untyped __call__("curl_multi_remove_handle", mh, c.h);
	}
}