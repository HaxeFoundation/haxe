package issues;

extern class Vim {
	static function list_extend<T>(target:Array<T>, source:Array<T>):Array<T>;
}

class Issue10908 {
	@:js('
		return issues.Vim.list_extend(issues.Vim.list_extend([],tableA),tableB);
	')
	static function test<T>(tableA:Array<T>, tableB:Array<T>):Array<T> {
		final result = Vim.list_extend([], tableA);
		return Vim.list_extend(result, tableB);
	}
}