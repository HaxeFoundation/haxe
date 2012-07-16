package cs.system;

@:native("System.Converter") 
@:delegate extern typedef Converter<TInput,TOutput> = TInput->TOutput;