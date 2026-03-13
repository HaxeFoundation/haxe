package languageServerProtocol.protocol;

import jsonrpc.Protocol.ProgressType;
import languageServerProtocol.protocol.Protocol.NotificationType;
import languageServerProtocol.protocol.Protocol.RequestType;

abstract ProtocolRequestType<P, R, PR, E, RO>(String) {
	public inline function new(method:String) {
		this = method;
	}

	@:to inline function toRequestType() {
		return new RequestType<P, R, E, RO>(this);
	}

	@:to inline function toProgressType() {
		return new ProgressType<PR>();
	}
}

abstract ProtocolNotificationType<P, RO>(String) {
	public inline function new(method:String) {
		this = method;
	}

	@:to inline function toNotificationType() {
		return new NotificationType<P, RO>(this);
	}
}

enum Never {}
