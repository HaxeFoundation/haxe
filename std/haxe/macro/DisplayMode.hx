package haxe.macro;

enum DisplayMode {
	None;
	Default;
	Definition;
	TypeDefinition;
	Implementation;
	Package;
	Hover;
	References(withDefinition:Bool, findDescendants:Bool, findBase:Bool);
	ModuleSymbols;
	WorkspaceSymbols(filter:String);
	Signature;
}
