enum abstract MyEnumAbstract(Int) {
	var ONE = 1;

	@:to
	private function toValue():String {
		return switch (abstract) {
			case ONE:
				"One";
		}
	}
}
