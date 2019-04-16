private enum abstract A(Int) { }

class Main {
	static public function main() {
		var a:Null<A> = cast 1;
		var b = switch(a) { }
	}
}