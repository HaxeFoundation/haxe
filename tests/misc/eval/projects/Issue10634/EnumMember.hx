enum E { A; B; }

function take(f:()->Void) {}

function main() {
	take(() -> {
		var e:E = NotACtor;
	});
}
