package cs.system;

@:native("System.Environment.SpecialFolderOption")
extern enum abstract Environment_SpecialFolderOption(Int) {
	var Create = 32768;
	var DoNotVerify = 16384;
	var None = 0;
}
