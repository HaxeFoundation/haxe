class Test {
	var value:String;

	public function new() {
		value = "I'm supposed to be private";
	}
}

function main() {
	var test = new Test();
	trace(test.value);
}