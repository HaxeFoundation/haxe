class MyMacro {
    var data:FunctionData;

    public function new(data:FunctionData) {
        this.data = data;
    }

    public function build():Void {
        {-1-}data.args.{-2-}
    }

}

@:structInit class FunctionData {
	public final args:Array<String>;
}
