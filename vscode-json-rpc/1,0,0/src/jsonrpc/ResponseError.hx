package jsonrpc;

import jsonrpc.Types;

abstract ResponseError<T>(ResponseErrorData) to ResponseErrorData {
	/**
		Invalid JSON was received by the server.
		An error occurred on the server while parsing the JSON text.
	**/
	public static inline var ParseError = -32700;

	/**
		The JSON sent is not a valid Request object.
	**/
	public static inline var InvalidRequest = -32600;

	/**
		The method does not exist / is not available.
	**/
	public static inline var MethodNotFound = -32601;

	/**
		Invalid method parameter(s).
	**/
	public static inline var InvalidParams = -32602;

	/**
		Internal JSON-RPC error.
	**/
	public static inline var InternalError = -32603;
	public static inline var ServerNotInitialized = -32002;
	public static inline var UnknownErrorCode = -32001;

	public inline function new(code:Int, message:String, ?data:Dynamic) {
		this = {code: code, message: message};
		if (data != null)
			this.data = data;
	}

	public static inline function internalError(message:String):ResponseError<NoData> {
		return new ResponseError(InternalError, message);
	}
}
