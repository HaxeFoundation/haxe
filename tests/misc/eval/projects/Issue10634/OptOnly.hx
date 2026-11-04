function take(?cb:()->Void) {}

function main() {
	take(() -> {
		doesNotExist();
	});
}
