@:coroutine function someOtherCoro() {}

@:coroutine.scope(restrictedSuspension)
class MyScope {
	@:coroutine public function scopeCoro() {}
}

function main() {
	@:coroutine function run(scope:MyScope) {
		scope.scopeCoro(); // this is allowed
		someOtherCoro(); // this is not
	}
}