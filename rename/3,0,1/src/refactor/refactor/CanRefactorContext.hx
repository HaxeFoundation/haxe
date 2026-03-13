package refactor.refactor;

typedef CanRefactorContext = CacheAndTyperContext & {
	var what:RefactorWhat;
}
