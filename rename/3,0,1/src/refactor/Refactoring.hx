package refactor;

import refactor.refactor.CanRefactorContext;
import refactor.refactor.CanRefactorResult;
import refactor.refactor.ExtractConstructorParams;
import refactor.refactor.ExtractInterface;
import refactor.refactor.ExtractMethod;
import refactor.refactor.ExtractType;
import refactor.refactor.RefactorContext;
import refactor.refactor.RefactorType;
import refactor.refactor.RewriteVarsToFinals;
import refactor.refactor.RewriteWrapWithTryCatch;

class Refactoring {
	public static function canRefactor(refactorType:RefactorType, context:CanRefactorContext, isRangeSameScope:Bool):CanRefactorResult {
		switch (refactorType) {
			case RefactorExtractInterface:
				return ExtractInterface.canRefactor(context, isRangeSameScope);
			case RefactorExtractMethod:
				return ExtractMethod.canRefactor(context, isRangeSameScope);
			case RefactorExtractType:
				return ExtractType.canRefactor(context, isRangeSameScope);
			case RefactorExtractConstructorParams(asFinal):
				return ExtractConstructorParams.canRefactor(context, isRangeSameScope, asFinal);
			case RefactorRewriteVarsToFinals(toFinals):
				return RewriteVarsToFinals.canRefactor(context, isRangeSameScope, toFinals);
			case RefactorRewriteWrapWithTryCatch:
				return RewriteWrapWithTryCatch.canRefactor(context, isRangeSameScope);
		}
		return Unsupported;
	}

	public static function doRefactor(refactorType:RefactorType, context:RefactorContext):Promise<RefactorResult> {
		switch (refactorType) {
			case RefactorExtractInterface:
				return ExtractInterface.doRefactor(context);
			case RefactorExtractMethod:
				return ExtractMethod.doRefactor(context);
			case RefactorExtractType:
				return ExtractType.doRefactor(context);
			case RefactorExtractConstructorParams(asFinal):
				return ExtractConstructorParams.doRefactor(context, asFinal);
			case RefactorRewriteVarsToFinals(toFinals):
				return RewriteVarsToFinals.doRefactor(context, toFinals);
			case RefactorRewriteWrapWithTryCatch:
				return RewriteWrapWithTryCatch.doRefactor(context);
		}
		return Promise.reject("no refactor type selected");
	}
}
