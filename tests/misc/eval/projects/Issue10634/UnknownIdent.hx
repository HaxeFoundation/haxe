function take(f:()->Void) {}

function main() {
	take(() -> {
		doesNotExist();
	});
}
