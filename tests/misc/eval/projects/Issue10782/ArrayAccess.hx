class C {
	public function new() {}

	public function call() {
		trace("called!");
	}
}

function main() {
	final f = function() {
		trace("f!");
	}
	[new C()][0].call();
}
