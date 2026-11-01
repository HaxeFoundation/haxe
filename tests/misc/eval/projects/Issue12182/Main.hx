abstract A(String) {
	public function new(s:String) {
		this = s;
	}
}

function main() {
	var a = new A("foo");
	var b = null;
	b.field = 1;
	a == b;
}