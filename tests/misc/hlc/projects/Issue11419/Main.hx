/**
	"SOCKET_ERROR" is reserved as a macro in windows.h, but hopefully our
	code shouldn't be affected.
**/
final SOCKET_ERROR = "SOCKET_ERROR";

function main() {
	trace(SOCKET_ERROR);
}
