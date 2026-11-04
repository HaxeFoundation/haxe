class Main {
	static function main() {
		breakEverything();
	}

	static macro function breakEverything() {
		eval.vm.Context.callMacroApi("oh no");
		return macro null;
	}
}