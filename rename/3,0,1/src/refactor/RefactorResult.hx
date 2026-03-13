package refactor;

enum RefactorResult {
	NoChange;
	NotFound;
	Unsupported(name:String);
	DryRun;
	Done;
}
