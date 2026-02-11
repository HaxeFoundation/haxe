@:coroutine function someOtherCoro() {}

@:coroutine.restrictedSuspension
class MyScope {
	@:coroutine public function scopeCoro() {}
}

function main() {
	@:coroutine function run(scope:MyScope) {
		scope.scopeCoro(); // this is allowed
		someOtherCoro(); // this is not
	}
}