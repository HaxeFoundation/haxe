@:generic
class StateHandler<S> {
	public var state(default, set):S;

	public function new() {}

	inline function set_state(state) {
		trace("State changed in the handler!");

		return this.state = state;
	}
}
