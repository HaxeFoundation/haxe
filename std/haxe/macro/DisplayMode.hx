package haxe.macro;

enum DisplayMode {
	None;
	Default;
	Definition;
	TypeDefinition;
	Implementation;
	Package;
	Hover;
	Rename(findDescendants:Bool, findBase:Bool);
	References(findDescendants:Bool, findBase:Bool);
	ModuleSymbols;
	WorkspaceSymbols(filter:String);
	Signature;
}
