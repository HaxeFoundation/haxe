/*
 * Copyright (c) 2006, Motion-Twin
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY MOTION-TWIN "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package mtwin.net;

import neko.net.Socket;
import neko.net.Host;

/**
	Handles FTP protocol.

	Example: [

		var ftp = new Ftp("my.ftp.com", 21);
		ftp.login("myuser", "mypass");
		ftp.cwd("uploads");
		neko.Lib.println(ftp.list());

		var fp = neko.io.File.read("localFile.dat", true);
		ftp.put(fp, "remoteName.dat");
		fp.close();

		fp = neko.io.File.write("copyFile.dat", true);
		ftp.get(fp, "remoteName2.dat");
		fp.close();

		ftp.close();
	]
**/
class Ftp {

	static var CRLF = "\r\n";

	public var debug : Bool;
	var host : Host;
	var port : Int;
	var user : String;
	var pass : String;
	var acct : String;
	var socket : Socket;
	var srv : Socket;
	var passiveMode : Bool;

	/**
		Creates a connection to an FTP server.
	**/
	public function new( host:String, ?port:Int ){
		debug = false;
		passiveMode = true;
		this.host = new Host(host);
		this.port = if (port != null) port else 21;
		socket = new Socket();
		socket.connect(this.host, this.port);
		var welcome = getLines();
	}

	/**
		Log into FTP server.
	**/
	public function login( ?login:String, ?pass:String, ?acct:String ){
		if (login == null) login = "anonymous";
		if (pass == null) pass = "";
		if (acct == null) acct = "";
		this.user = login;
		this.pass = pass;
		this.acct = acct;
		var res = command("USER "+login);
		if (res.charAt(0) == "3") res = command("PASS "+pass);
		if (res.charAt(0) == "3") res = command("ACCT "+acct);
		if (res.charAt(0) != "2")
			throw res;
	}

	/**
		Enable/Disable passive mode (default is on).
	**/
	public function setPassiveMode( b:Bool ){
		passiveMode = b;
	}

	/**
		Returns remote current working directory.
	**/
	public function pwd() : String {
		var res = command("PWD");
		var re = ~/257 "(.*?)"/;
		if (re.match(res))
			return re.matched(1);
		throw res;
	}

	/**
		Change remote current working directory.
	**/
	public function cwd( path:String ){
		if (path == ".."){
			voidCommand("CDUP");
			return;
		}
		if (path == "")
			path = ".";
		voidCommand("CWD "+path);
	}

	/**
		Create a remote directory.
	**/
	public function createDirectory( path: String ){
		var res = command("MKD "+path);
		var re = ~/257 "(.*?)"/;
		if (re.match(res))
			return true;
		throw res;
	}

	/**
		Delete remove directory.
	**/
	public function removeDirectory( path:String ){
		voidCommand("RMD "+path);
	}

	/**
		Retrieve remote file size.
	**/
	public function fileSize( path:String ) : Int {
		voidCommand("TYPE I");
		var res = command("SIZE "+path);
		voidCommand("TYPE A");
		if (res.substr(0,3) != "213")
			throw res;
		res = StringTools.trim(res.substr(3,res.length));
		return Std.parseInt(res);
	}

	/**
		Rename remote file or directory.
	**/
	public function rename( from:String, to:String ) {
		var res = command("RNFR "+from);
		if (res.charAt(0) != "3")
			throw res;
		voidCommand("RNTO "+to);
	}

	/**
		Delete specified file from FTP server.
	**/
	public function deleteFile( path:String ) {
		var res = command("DELE "+path);
		if (res.substr(0,3) != "200" && res.substr(0,3) != "250")
			throw res;
	}

	/**
		Returns a quick listing of specified directory (or current directory if args omited).
	**/
	public function list( ?args:String ) : Array<String> {
		var cmd = "NLST "+if (args != null) args else "";
		return retrieveLines(cmd);
	}

	/**
		Returns a detailed listing of specified directory (or current directory if args omited).
	**/
	public function detailedList( ?args:String ) : Array<String> {
		var cmd = "LIST "+if (args != null) args else "";
		return retrieveLines(cmd);
	}

	/**
		Reads input and upload its content to remoteName.
	**/
	public function put( input:haxe.io.Input, remoteName:String, ?bufSize:Int ){
		if (bufSize == null)
			bufSize = 8192;
		voidCommand("TYPE I");
		var cnx = transferConnection("STOR "+remoteName);
		cnx.output.writeInput(input, bufSize);
		cnx.close();
		voidResponse();
	}

	/**
		Downloads remote file and write its content in output using neko.io.Output.writeBytes()
	**/
	public function get( output:haxe.io.Output, remoteFileName:String, ?bufSize:Int ){
		if (bufSize == null)
			bufSize = 8192;
		retrieveBytes("RETR "+remoteFileName, function(s:haxe.io.Bytes, n:Int){ output.writeBytes(s,0,n); }, bufSize);
	}

	/**
		Close connection.
	**/
	public function close(){
		command("QUIT");
		socket.close();
		if (srv != null)
			srv.close();
	}

	/**
		Creates a new FTP connection dedicated to write to specified file.

		Don't forget to close the neko.io.Output when done.
	**/
	public function write( remoteName:String ) : haxe.io.Output {
		var pwd = pwd();
		var ftp = new Ftp(host.toString(), port);
		ftp.login(user, pass, acct);
		ftp.cwd(pwd);
		ftp.voidCommand("TYPE I");
		var cnx = ftp.transferConnection("STOR "+remoteName);
		var out = cnx.output;
		var old = out.close;
		(cast out).close = function(){
			old();
			ftp.voidResponse();
			ftp.close();
		}
		return out;
	}

	/**
		Creates a new FTP connection dedicated to read specified file.

		Don't forget to close the neko.io.Output when done.
	**/
	public function read( remoteFileName:String ) : haxe.io.Input {
		var pwd = pwd();
		var ftp = new Ftp(host.toString(), port);
		ftp.login(user, pass, acct);
		ftp.cwd(pwd);
		ftp.voidCommand("TYPE I");
		var cnx = ftp.transferConnection("RETR "+remoteFileName);
		var inp = cnx.input;
		var old = inp.close;
		(cast inp).close = function(){
			old();
			ftp.voidResponse();
			ftp.close();
		}
		return inp;
	}

	function voidResponse() : String {
		var res = getLines().join("\n");
		if (debug) trace("VR< "+res);
		if (res.charAt(0) != "2")
			throw res;
		return res;
	}

	function voidCommand( command:String ) : String {
		if (debug) trace("VC> "+command);
		socket.write(command + CRLF);
		return voidResponse();
	}

	function command( command:String ) : String {
		if (debug) trace("C> "+command);
		socket.write(command + CRLF);
		return getLines().pop();
		//return getLine();
	}

	function getLine() : String {
		var r = socket.input.readLine();
		if (debug) trace(r);
		return r;
	}

	function getLines() : Array<String> {
		var lines = new Array();
		var line = getLine();
		lines.push(line);
		if (line.charAt(3) != '-'){
			return lines;
		}
		while (true){
			line = getLine();
			if (line.charAt(3) != "-"){
				var code = line.substr(0, 3);
				break;
			}
			else {
				lines.push(line);
			}
		}
		return lines;
	}

	function getPassivePort() : Int {
		var res = command("PASV");
		if (res.substr(0,3) != "227")
			throw "Not PASV response : "+res;
		var reg = ~/(\d+),(\d+),(\d+),(\d+),(\d+),(\d+)/;
		if (reg.match(res)){
			var port = (Std.parseInt(reg.matched(5)) << 8) + Std.parseInt(reg.matched(6));
			return port;
		}
		throw "Unable to activate PASV : "+res;
	}

	function transferConnection( cmd:String, ?rest:String ) : Socket {
		var cnx = null;
		if (passiveMode){
			var tport = getPassivePort();
			cnx = new Socket();
			cnx.connect(host, tport);
			if (rest != null) command("REST "+rest);
			var res = command(cmd);
			// transfer complete there means nothing do do
			if (res.substr(0,3) == "226"){
				cnx.close();
				return null;
			}
			else if (res.charAt(0) != "1")
				throw res;
		}
		else {
			if (srv != null)
				srv.close();
			srv = createTransferServer();
			if (rest != null)
				command("REST "+rest);
			var res = command(cmd);
			if (res.charAt(0) != "1")
				throw res;
			cnx = srv.accept();
		}
		return cnx;
	}

	function retrieveLines( cmd:String, ?rest:String ) : Array<String> {
		var res = command("TYPE A");
		var cnx = transferConnection(cmd, rest);
		// nothing to retrieve
		if (cnx == null)
			return [];
		var lines = new Array();
		while (true){
			try {
				var line = cnx.input.readLine();
				lines.push(line);
			}
			catch (eof:haxe.io.Eof){
				cnx.close();
				voidResponse();
				return lines;
			}
		}
		cnx.close();
		throw voidResponse();
		return null;
	}

	function retrieveBytes( cmd:String, cb:haxe.io.Bytes->Int->Void, ?bufSize:Int, ?rest:String ) {
		if (bufSize == null)
			bufSize = 8192;
		var res = voidCommand("TYPE I");
		var cnx = transferConnection(cmd, rest);
		var buf = haxe.io.Bytes.alloc(bufSize);
		while (cnx != null){
			var rdd = try cnx.input.readBytes(buf, 0, bufSize) catch(eof:haxe.io.Eof) break;
			cb(buf, rdd);
			/*
			var rdd = 0;
			try {
				var buf = neko.Lib.makeString(bufSize);
				rdd = cnx.input.readBytes(buf, 0, bufSize);
				cb(buf, rdd);
			}
			catch (eof:Eof){
				rdd = 0;
			}
			if (rdd < bufSize)
				break;
			*/
		}
		if (cnx != null)
			cnx.close();
		voidResponse();
	}

	function createTransferServer(){
		var sock = null;
		var port = 0;
		while (true){
			try {
				port = 1025 + Std.random(9999);
				sock = new Socket();
				sock.bind(socket.host().host, port);
			}
			catch (e:Dynamic){
				sock.close();
				sock = null;
			}
			if (sock != null)
				break;
		}
		sock.listen(1);
		var hostIp = StringTools.replace(socket.host().host.toString(), ".", ",");
		voidCommand("PORT "+hostIp+","+Std.int(port/256)+","+(port%256));
		return sock;
	}
}
