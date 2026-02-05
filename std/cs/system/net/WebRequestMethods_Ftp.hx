package cs.system.net;

@:native("System.Net.WebRequestMethods.Ftp")
extern class WebRequestMethods_Ftp {
	static var AppendFile(default, never):String;
	static var DeleteFile(default, never):String;
	static var DownloadFile(default, never):String;
	static var GetDateTimestamp(default, never):String;
	static var GetFileSize(default, never):String;
	static var ListDirectory(default, never):String;
	static var ListDirectoryDetails(default, never):String;
	static var MakeDirectory(default, never):String;
	static var PrintWorkingDirectory(default, never):String;
	static var RemoveDirectory(default, never):String;
	static var Rename(default, never):String;
	static var UploadFile(default, never):String;
	static var UploadFileWithUniqueName(default, never):String;
}
