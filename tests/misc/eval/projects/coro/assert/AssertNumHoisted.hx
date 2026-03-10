class AssertNumStates {

	@:coroutine(assert = {numHoisted: 1})
	static function f():Void { }

	static function main() {}
}
