function main() {
	Sys.command("jar", ["cf", "test.jar", "pack/ItemGroupEvents.class", "pack/ItemGroupEvents$ModifyEntries.class"]);
}
