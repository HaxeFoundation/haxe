import haxe.coro.Coroutine;

@:coroutine function someOtherCoro() {}

@:coroutine.restrictedSuspension
typedef MyScope = Coroutine<() -> Void>;

function main() {
	@:coroutine function run(scope:MyScope) {
		scope(); // this is allowed
		someOtherCoro(); // this is not
	}
}