package cases;

import Validator.shouldFail;

// ---- Issue #12459: the vshaxe protocol pattern ----
// Reproduces the false-positive "Null<X> should be Dynamic" error when type
// parameters are resolved through generic instantiation.

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

// ---- Generic classes with Dynamic type parameter ----

class GenericBox<T> {
	public var value:T;

	public function new(v:T) {
		this.value = v;
	}
}

@:nullSafety
class TestGenericDynamic {
	// Instantiating Generic<Dynamic> with a nullable argument should pass
	static function nullableToGenericDynamic(?n:String) {
		new GenericBox<Dynamic>(n);
	}

	// Instantiating Generic<String> with a nullable argument should fail
	static function nullableToGenericString(?n:String) {
		shouldFail(new GenericBox<String>(n));
	}

	// Deeply nullable value to Generic<Dynamic> should pass
	static function deeplyNullableToGenericDynamic(?n:Null<Array<Null<String>>>) {
		new GenericBox<Dynamic>(n);
	}
}

// ---- Collections with Dynamic element type ----

@:nullSafety
class TestCollectionDynamic {
	// Array<Dynamic> should accept nullable elements
	static function nullableToArrayDynamic(?a:String) {
		var arr:Array<Dynamic> = [a];
	}

	// Pre-typed Array<Null<T>> assigned to Array<Dynamic> should pass
	// (goes through the type parameter unificator path)
	static function preTypedNullableArrayToDynamic() {
		var nullableArr:Array<Null<String>> = [null];
		var dArr:Array<Dynamic> = nullableArr;
	}

	// Array<String> should still reject nullable elements
	static function nullableToArrayString(?a:String) {
		var withNullable = [a];
		shouldFail(var arr:Array<String> = withNullable);
	}

	// Map<String, Dynamic> should accept nullable values
	static function nullableToMapDynamic(?v:String) {
		var m:Map<String, Dynamic> = ["key" => v];
	}

	// Pre-typed Map<String, Null<T>> assigned to Map<String, Dynamic> should pass
	static function preTypedNullableMapToDynamic() {
		var nullableMap:Map<String, Null<Int>> = ["key" => null];
		var dMap:Map<String, Dynamic> = nullableMap;
	}

	// Map<String, String> should still reject nullable values
	static function nullableToMapString(?v:String) {
		var withNullable = ["key" => v];
		shouldFail(var m:Map<String, String> = withNullable);
	}
}

// ---- Multiple type parameters, some Dynamic and some not ----

class Pair<A, B> {
	public var a:A;
	public var b:B;

	public function new(a:A, b:B) {
		this.a = a;
		this.b = b;
	}
}

@:nullSafety
class TestMultiTypeParams {
	// Dynamic first parameter accepts nullable, non-nullable second requires non-null
	static function nullableToFirstDynamicParam(?a:String) {
		new Pair<Dynamic, String>(a, "safe");
	}

	// Non-nullable first parameter rejects nullable
	static function nullableToFirstStringParam(?a:String) {
		shouldFail(new Pair<String, Dynamic>(a, "safe"));
	}

	// Dynamic second parameter accepts nullable
	static function nullableToSecondDynamicParam(?b:String) {
		new Pair<String, Dynamic>("safe", b);
	}

	// Both Dynamic parameters accept nullable values
	static function nullableToBothDynamicParams(?a:String, ?b:Int) {
		new Pair<Dynamic, Dynamic>(a, b);
	}
}

// ---- Deeply nested / "insane" type parameters ----
// These test the fix across multiple levels of generic indirection.

class DeepRequest<A, B, C> {
	public function new() {}
}

@:nullSafety
class DeepProtocol {
	public function new() {}

	public function handle<A, B, C>(
		req:DeepRequest<A, B, C>,
		handler:(Dynamic, Dynamic, Dynamic) -> Void
	):Void {}
}

@:nullSafety
class TestDeepTypeParams {
	// Three levels of nullable type params unified with Dynamic handler args
	static function tripleNullableToDeepDynamic(?a:String, ?b:Int) {
		final proto = new DeepProtocol();
		final req = new DeepRequest<Null<String>, Null<Int>, Null<Array<Null<String>>>>();
		proto.handle(req, (x, y, z) -> {});
	}

	// RequestType with a deeply-nested nullable type parameter
	static function deeplyNestedNullableRequestType() {
		final protocol = new Protocol();
		final reqType = new RequestType<Null<Array<Null<String>>>>();
		protocol.onRequest(reqType, resolve -> {
			resolve(null);
		});
	}

	// Wrapper<T> where T is resolved to a complex nested nullable type
	static function complexNestedNullableToProtocolHandler() {
		final req = new RequestType<Null<Map<String, Null<Int>>>>();
		final protocol = new Protocol();
		// handler: ((v:Dynamic)->Void)->Void, type param T = Null<Map<String,Null<Int>>>
		protocol.onRequest(req, resolve -> {
			resolve(null);
		});
	}
}

// Inlining a generic function returning Null<T> must not bind the inline
// type-param monomorph to Null<Int> instead of Int.
@:nullSafety
class TestInlineGenericReturn {
	static function main() {
		final arr = [1, 2, 3];
		iter(arr, item -> true);
	}

	static inline function iter<T>(it:Array<T>, callback:(item:T) -> Bool):Null<T> {
		for (v in it) callback(v);
		return null;
	}
}
