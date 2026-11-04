function main() {
	final good = lua.Table.create([4, 1, 3], {"hello": "world"});

	final array = [4, 1, 3];
	final obj = {"hello":  "world"};

	final bad = lua.Table.create(array, obj);
}
