package hxjsonast;

/**
    Error object containing message and position.

    Thrown by the `Parser`, but can be used in user's code as well.
**/
class Error {
    public var message:String;
    public var pos:Position;

    public function new(message:String, pos:Position) {
        this.message = message;
        this.pos = pos;
    }
}
