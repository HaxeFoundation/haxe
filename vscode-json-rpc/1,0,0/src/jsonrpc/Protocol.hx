package jsonrpc;

import haxe.extern.EitherType;
import jsonrpc.CancellationToken;
import jsonrpc.ErrorUtils.errorToString;
import jsonrpc.Types;

typedef CancelParams = {
	/**
		The request id to cancel.
	**/
	var id:EitherType<Int, String>;
}

class CancelNotification {
	public static inline final type = new NotificationType<CancelParams>("$/cancelRequest");
}

typedef ProgressToken = EitherType<Int, String>;

typedef ProgressParams<T> = {
	/**
		The progress token provided by the client.
	**/
	var token:ProgressToken;

	/**
		The progress data.
	**/
	var value:T;
}

class ProgressNotification {
	public static inline final type = new NotificationType<ProgressParams<Any>>("$/progress");
}

class ProgressType<P> {
	public function new() {}
}

typedef RequestHandler<P, R, E> = (params:P, token:CancellationToken, resolve:(response:R) -> Void, reject:(error:ResponseError<E>) -> Void) -> Void;
typedef NotificationHandler<P> = (params:P) -> Void;

typedef Disposable = {
	/**
		Dispose this object.
	**/
	function dispose():Void;
}

/**
	A simple JSON-RPC protocol base class.
**/
class Protocol {
	public static inline var PROTOCOL_VERSION = "2.0";

	public var didRespondToRequest:Null<(request:RequestMessage, response:ResponseMessage) -> Void>;
	public var didSendNotification:Null<(notification:NotificationMessage) -> Void>;

	var writeMessage:(message:Message, token:Null<CancellationToken>) -> Void;
	var requestTokens:Map<String, CancellationTokenSource>;
	var nextRequestId:Int;
	var requestHandlers:Map<String, RequestHandler<Dynamic, Dynamic, Dynamic>>;
	var notificationHandlers:Map<String, NotificationHandler<Dynamic>>;
	// note: using an ObjectMap here is not safe on all targets
	var progressHandlers:Map<{}, NotificationHandler<Dynamic>>;

	var responseCallbacks:Map<Int, ResponseCallbackEntry>;

	public function new(writeMessage) {
		this.writeMessage = writeMessage;
		requestTokens = new Map();
		nextRequestId = 0;
		requestHandlers = new Map();
		notificationHandlers = new Map();
		progressHandlers = new Map();
		responseCallbacks = new Map();

		onNotification(ProgressNotification.type, function(params) {
			var handler = progressHandlers.get(params.token);
			if (handler != null) {
				handler(params.value);
			} else {
				// unhandledProgressEmitter.fire(params);
			}
		});
	}

	public function handleMessage(message:Message):Void {
		if ((Reflect.hasField(message, "result") || Reflect.hasField(message, "error")) && Reflect.hasField(message, "id")) {
			handleResponse(cast message);
		} else if (Reflect.hasField(message, "method")) {
			if (Reflect.hasField(message, "id"))
				handleRequest(cast message);
			else
				handleNotification(cast message);
		}
	}

	public inline function onRequest<P, R, E>(method:RequestType<P, R, E>, handler:RequestHandler<P, R, E>):Void {
		requestHandlers[method] = handler;
	}

	public inline function onNotification<P>(method:NotificationType<P>, handler:NotificationHandler<P>):Void {
		notificationHandlers[method] = handler;
	}

	public function onProgress<P>(type:ProgressType<P>, token:ProgressToken, handler:NotificationHandler<P>):Disposable {
		if (progressHandlers.exists(token)) {
			throw 'Progress handler for token $token already registered';
		}
		progressHandlers[token] = handler;
		return {
			dispose: function() {
				progressHandlers.remove(token);
			}
		}
	}

	function handleRequest(request:RequestMessage) {
		var tokenKey = Std.string(request.id);

		function resolve(result:Dynamic) {
			requestTokens.remove(tokenKey);

			var response:ResponseMessage = {
				jsonrpc: PROTOCOL_VERSION,
				id: request.id,
				result: result
			};
			writeMessage(response, null);

			if (didRespondToRequest != null) {
				didRespondToRequest(request, response);
			}
		}

		function reject(error:ResponseErrorData) {
			requestTokens.remove(tokenKey);

			var response:ResponseMessage = {
				jsonrpc: PROTOCOL_VERSION,
				id: request.id,
				error: error
			};
			writeMessage(response, null);
		}

		var handler = requestHandlers[request.method];
		if (handler == null)
			return reject(new ResponseError(ResponseError.MethodNotFound, 'Unhandled method ${request.method}'));

		var tokenSource = new CancellationTokenSource();
		requestTokens[tokenKey] = tokenSource;

		try {
			handler(request.params, tokenSource.token, resolve, reject);
		} catch (e:Dynamic) {
			requestTokens.remove(tokenKey);

			var message = errorToString(e, 'Exception while handling request ${request.method}: ');
			reject(ResponseError.internalError(message));
			logError(message);
		}
	}

	function handleNotification(notification:NotificationMessage) {
		if (notification.method == CancelNotification.type) {
			var tokenKey = Std.string(notification.params.id);
			var tokenSource = requestTokens[tokenKey];
			if (tokenSource != null) {
				requestTokens.remove(tokenKey);
				tokenSource.cancel();
			}
		} else {
			var handler = notificationHandlers[notification.method];
			if (handler == null)
				return;
			try {
				handler(notification.params);
			} catch (e:Dynamic) {
				logError(errorToString(e, 'Exception while processing notification ${notification.method}: '));
			}
		}
	}

	function handleResponse(response:ResponseMessage) {
		if (!(response.id is Int)) {
			logError("Got response with non-integer id:\n" + haxe.Json.stringify(response, "    "));
			return;
		}
		var handler = responseCallbacks[response.id];
		if (handler != null) {
			responseCallbacks.remove(response.id);
			try {
				if (Reflect.hasField(response, "error")) {
					if (handler.reject != null) {
						handler.reject(response.error);
					}
				} else {
					if (handler.resolve != null) {
						handler.resolve(response.result);
					}
				}
			} catch (e:Dynamic) {
				logError(errorToString(e, 'Exception while handing response ${handler.method}: '));
			}
		}
	}

	public inline function sendNotification<P>(name:NotificationType<P>, ?params:P):Void {
		var message:NotificationMessage = {
			jsonrpc: PROTOCOL_VERSION,
			method: name
		};
		if (params != null)
			message.params = params;
		writeMessage(message, null);

		if (didSendNotification != null) {
			didSendNotification(message);
		}
	}

	public inline function sendProgress<P>(type:ProgressType<P>, token:ProgressToken, value:P):Void {
		sendNotification(ProgressNotification.type, {token: token, value: value});
	}

	public function sendRequest<P, R, E>(method:RequestType<P, R, E>, params:P, ?token:CancellationToken, ?resolve:(result:R) -> Void,
			?reject:(error:E) -> Void):Void {
		var id = nextRequestId++;
		var request:RequestMessage = {
			jsonrpc: PROTOCOL_VERSION,
			id: id,
			method: method,
		};
		if (params != null)
			request.params = params;
		responseCallbacks[id] = new ResponseCallbackEntry(method, resolve, reject);
		if (token != null)
			token.setCallback(() -> sendNotification(CancelNotification.type, {id: id}));
		writeMessage(request, token);
	}

	public dynamic function logError(message:String):Void {}
}

private class ResponseCallbackEntry {
	public var method:String;
	public var resolve:Null<Dynamic->Void>;
	public var reject:Null<Dynamic->Void>;

	public function new(method, resolve, reject) {
		this.method = method;
		this.resolve = resolve;
		this.reject = reject;
	}
}
