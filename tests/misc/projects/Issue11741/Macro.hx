
import haxe.macro.Expr.Field;
import haxe.macro.Context;

class Macro {
    public static macro function buildMain(): Array<Field> {
        final pos = Context.currentPos();
        final bf = Context.getBuildFields();
    
        final type = Context.getType('Test2');
    
        switch (type) {
            case TInst(ctRef, _):
                final ct = ctRef.get();
                
                try {
                    final ctor = ct.constructor.get().expr();
                } catch (e: Dynamic) {

                }
                
                Context.warning('This does not show if printStuff() calls error() or reportError()', pos);
            case _:
        }
    
        return bf;
    }

    public static macro function buildTest2(): Array<Field> {
        final pos = Context.currentPos();
        final bf = Context.getBuildFields();
        
        // Use error() or reportError() here -> Main Completion does not work, Eval crashes
        // Use warning() or info() -> Everything works as expected
        Context.reportError('Crashes HERE in eval.vm.callMacroApi', pos);

        return bf;
    }
}
