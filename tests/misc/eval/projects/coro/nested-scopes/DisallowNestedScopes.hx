@:coroutine.scope
class MyScope {}

function main() {
	@:coroutine function outer(scope:MyScope) {
		@:coroutine function inner() {
			// inner has no own scope, so this is fine
			trace(scope);
		}
	}

	@:coroutine function outer(scope:MyScope) {
		@:coroutine function inner(otherScope:MyScope) {
			// inner has own scope, should error
			trace(scope);
		}
	}
}