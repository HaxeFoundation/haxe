package callstack;

function throwing() {
	throw new haxe.exceptions.NotImplementedException();
}

function topCall2() {
	throwing();
}

function topCall1() {
	topCall2();
}