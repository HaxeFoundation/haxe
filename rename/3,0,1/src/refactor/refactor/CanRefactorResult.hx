package refactor.refactor;

enum CanRefactorResult {
	Unsupported;
	Supported(title:String);
}
