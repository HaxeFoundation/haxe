package haxe.macro;

enum DisplayMode {
	None;
	Default;
	Definition;
	TypeDefinition;
	Implementation;
	Package;
	Hover;
	Usage(withDefinition:Bool, findDescendants:Bool, findBase:Bool);
	ModuleSymbols;
	WorkspaceSymbols(filter:String);
	Signature;
}
