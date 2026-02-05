package cs.system.net.http;

/** Indicates if  operations should be considered completed either as soon as a response is available, or after reading the entire response message including the content. */
@:native("System.Net.Http.HttpCompletionOption")
extern enum HttpCompletionOption {
	ResponseContentRead;
	ResponseHeadersRead;
}
