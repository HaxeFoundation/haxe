package cs.system.net.http;

/** A base type for handlers which only do some small processing of request and/or response messages. */
@:native("System.Net.Http.MessageProcessingHandler")
extern class MessageProcessingHandler extends cs.system.net.http.DelegatingHandler {
}
