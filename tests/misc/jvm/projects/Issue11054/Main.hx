import pack.ItemGroupEvents;

function test(m:ItemGroupEvents_ModifyEntries) {
	m.modifyEntries(12);
}

function main() {
	test(i -> trace(12));
}
