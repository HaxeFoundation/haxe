package cases;

@:nullSafety
class TestTypeParameters {
	static function main() {
		final protocol = new Protocol();
		protocol.onRequest(CompletionRequest.type, onCompletion);
	}

	static function onCompletion(resolve:(v:Null<String>)->Void) {}
}

@:nullSafety
class Protocol {
	public function new() {}
	public function onRequest<T>(
		type:RequestType<T>,
		handler:((v:Dynamic) -> Void) -> Void
	):Void {}
}

class RequestType<T> {
	public function new() {}
}

@:nullSafety
class CompletionRequest {
	public static var type = new RequestType<Null<String>>();
}
