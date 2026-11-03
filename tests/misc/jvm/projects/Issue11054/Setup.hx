function main() {
	Sys.command("javac", ["pack/ItemGroupEvents.java", "-g"]);
	Sys.command("jar", ["cf", "test.jar", "pack/ItemGroupEvents.class", "pack/ItemGroupEvents$ModifyEntries.class"]);
}
