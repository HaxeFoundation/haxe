pub enum StackItem {
	haxe::StackItem,
	FilePos(haxe::StackItem, Option<@str>, i32),
	Lambda(i32),
	Method(Option<@str>, Option<@str>),
	Module(Option<@str>)
}
