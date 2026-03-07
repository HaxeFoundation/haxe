import sys.io.File;

function main() {
	var dump = File.getContent("dump/decoding_error.txt");
	Sys.println(dump);
}
