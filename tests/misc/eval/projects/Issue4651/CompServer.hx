class CompServer {
    static function main() {
        var port = 4000;
        var server = new sys.io.Process("haxe", ["--wait", "" + port]);
        var socket = new sys.net.Socket();
        socket.connect(new sys.net.Host("localhost"), port);
        socket.write("--display Main.hx@43\n-D display-stdin");
        socket.write("\x01");
        socket.write(sys.io.File.getContent("Main.hx.stdin"));
        socket.write("\x00");
        var out = socket.read();
        socket.close();
        Sys.stderr().writeString(out);
        server.kill();
    }
}