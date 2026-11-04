class AssertNumStates {
	@:coroutine static function inner():Void {}

	@:coroutine(assert = {numStates: 1})
	static function f():Void {
		inner();
		inner();
	}

	static function main() {}
}
