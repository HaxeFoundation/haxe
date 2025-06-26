enum StateEnum {
	State1;
	State2;
}

class Main {
	var stateHandler = new StateHandler<StateEnum>();

	public function new() {
		stateHandler.state = State1;
	}

	public static function main() {
		new Main();
	}
}
