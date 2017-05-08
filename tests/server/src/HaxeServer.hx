import js.node.child_process.ChildProcess as ChildProcessObject;
import js.node.child_process.ChildProcess.ChildProcessEvent;
import js.node.Buffer;
import js.node.ChildProcess;
import js.node.stream.Readable;

using StringTools;

class ErrorUtils {
    public static function errorToString(error:Dynamic, intro:String):String {
        var result = intro + Std.string(error);
        var stack = haxe.CallStack.exceptionStack();
        if (stack != null && stack.length > 0)
            result += "\n" + haxe.CallStack.toString(stack);
        return result;
    }
}

private class DisplayRequest {
    // these are used for the queue
    public var prev:DisplayRequest;
    public var next:DisplayRequest;

    var args:Array<String>;
    var stdin:String;
    var callback:String->Void;
    var errback:String->Void;

    static var stdinSepBuf = new Buffer([1]);

    public function new(args:Array<String>, stdin:String, callback:String->Void, errback:String->Void) {
        this.args = args;
        this.stdin = stdin;
        this.callback = callback;
        this.errback = errback;
    }

    public function prepareBody():Buffer {
        if (stdin != null) {
            args.push("-D");
            args.push("display-stdin");
        }

        var lenBuf = new Buffer(4);
        var chunks = [lenBuf];
        var length = 0;
        for (arg in args) {
            var buf = new Buffer(arg + "\n");
            chunks.push(buf);
            length += buf.length;
        }

        if (stdin != null) {
            chunks.push(stdinSepBuf);
            var buf = new Buffer(stdin);
            chunks.push(buf);
            length += buf.length + stdinSepBuf.length;
        }

        lenBuf.writeInt32LE(length, 0);

        return Buffer.concat(chunks, length + 4);
    }

    public function processResult(data:String) {
        var buf = new StringBuf();
        var hasError = false;
        for (line in data.split("\n")) {
            switch (line.fastCodeAt(0)) {
                case 0x01: // print
                    var line = line.substring(1).replace("\x01", "\n");
                    trace("Haxe print:\n" + line);
                case 0x02: // error
                    hasError = true;
                default:
                    buf.add(line);
                    buf.addChar("\n".code);
            }
        }

        var data = buf.toString().trim();

        if (hasError)
            return errback(data);

        try {
            callback(data);
        } catch (e:Any) {
            errback(ErrorUtils.errorToString(e, "Exception while handling Haxe completion response: "));
        }
    }
}

typedef DisplayServerConfigBase = {
    var haxePath:String;
    var arguments:Array<String>;
    var env:haxe.DynamicAccess<String>;
}

typedef Context = {
    function sendErrorMessage(msg:String):Void;
    function sendLogMessage(msg:String):Void;
    var displayServerConfig:DisplayServerConfigBase;
}

private class MessageBuffer {
    static inline var DEFAULT_SIZE = 8192;

    var index:Int;
    var buffer:Buffer;

    public function new() {
        index = 0;
        buffer = new Buffer(DEFAULT_SIZE);
    }

    public function append(chunk:Buffer):Void {
        if (buffer.length - index >= chunk.length) {
            chunk.copy(buffer, index, 0, chunk.length);
        } else {
            var newSize = (Math.ceil((index + chunk.length) / DEFAULT_SIZE) + 1) * DEFAULT_SIZE;
            if (index == 0) {
                buffer = new Buffer(newSize);
                chunk.copy(buffer, 0, 0, chunk.length);
            } else {
                buffer = Buffer.concat([buffer.slice(0, index), chunk], newSize);
            }
        }
        index += chunk.length;
    }

    public function tryReadLength():Int {
        if (index < 4)
            return -1;
        var length = buffer.readInt32LE(0);
        buffer = buffer.slice(4);
        index -= 4;
        return length;
    }

    public function tryReadContent(length:Int):String {
        if (index < length)
            return null;
        var result = buffer.toString("utf-8", 0, length);
        var nextStart = length;
        buffer.copy(buffer, 0, nextStart);
        index -= nextStart;
        return result;
    }

    public function getContent():String {
        return buffer.toString("utf-8", 0, index);
    }
}

class HaxeServer {
    var proc:ChildProcessObject;

    var context:Context;
    var buffer:MessageBuffer;
    var nextMessageLength:Int;

    var requestsHead:DisplayRequest;
    var requestsTail:DisplayRequest;
    var currentRequest:DisplayRequest;

    public function new(context:Context) {
        this.context = context;
    }

    static var reTrailingNewline = ~/\r?\n$/;

    public function start() {
        stop();

        inline function error(s) context.sendErrorMessage(s);

        var env = new haxe.DynamicAccess();
        for (key in js.Node.process.env.keys())
            env[key] = js.Node.process.env[key];

        buffer = new MessageBuffer();
        nextMessageLength = -1;

        proc = ChildProcess.spawn(context.displayServerConfig.haxePath, context.displayServerConfig.arguments.concat(["--wait", "stdio"]), {env: env});

        proc.stdout.on(ReadableEvent.Data, function(buf:Buffer) {
            context.sendLogMessage(reTrailingNewline.replace(buf.toString(), ""));
        });
        proc.stderr.on(ReadableEvent.Data, onData);

        proc.on(ChildProcessEvent.Exit, onExit);
    }

    public function stop() {
        if (proc != null) {
            proc.removeAllListeners();
            proc.kill();
            proc = null;
        }

        requestsHead = requestsTail = currentRequest = null;
    }

    public function restart(reason:String) {
        context.sendLogMessage('Restarting Haxe completion server: $reason');
        start();
    }

    function onExit(_, _) {
        var haxeResponse = buffer.getContent();
        trace("\nError message from the compiler:\n");
        trace(haxeResponse);
    }

    function onData(data:Buffer) {
        buffer.append(data);
        while (true) {
            if (nextMessageLength == -1) {
                var length = buffer.tryReadLength();
                if (length == -1)
                    return;
                nextMessageLength = length;
            }
            var msg = buffer.tryReadContent(nextMessageLength);
            if (msg == null)
                return;
            nextMessageLength = -1;
            if (currentRequest != null) {
                var request = currentRequest;
                currentRequest = null;
                request.processResult(msg);
                checkQueue();
            }
        }
    }

    public function process(args:Array<String>, stdin:String, callback:String->Void, errback:String->Void) {
        // create a request object
        var request = new DisplayRequest(args, stdin, callback, errback);

        // add to the queue
        if (requestsHead == null) {
            requestsHead = requestsTail = request;
        } else {
            requestsTail.next = request;
            request.prev = requestsTail;
            requestsTail = request;
        }

        // process the queue
        checkQueue();
    }

    function checkQueue() {
        // there's a currently processing request, wait and don't send another one to Haxe
        if (currentRequest != null)
            return;

        // pop the first request still in queue, set it as current and send to Haxe
        if (requestsHead != null) {
            currentRequest = requestsHead;
            requestsHead = currentRequest.next;
            proc.stdin.write(currentRequest.prepareBody());
        }
    }
}