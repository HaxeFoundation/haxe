package refactor.refactor;

enum RefactorType {
	RefactorExtractInterface;
	RefactorExtractMethod;
	RefactorExtractType;
	RefactorExtractConstructorParams(asFinal:Bool);
	RefactorRewriteVarsToFinals(toFinals:Bool);
	RefactorRewriteWrapWithTryCatch;
}
