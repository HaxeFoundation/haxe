abstract UnrestrictedAbstract(Int) { }

@:coroutine.restrictedSuspension
abstract RestrictedAbstract(UnrestrictedAbstract) to UnrestrictedAbstract {
	@:coroutine public function test() {}
}

@:coroutine function passToUnretrictedAbstract(_:UnrestrictedAbstract) { }

function main() {
	@:coroutine function run(scope:RestrictedAbstract) {
		scope.test();
		passToUnretrictedAbstract(scope);
	}
}