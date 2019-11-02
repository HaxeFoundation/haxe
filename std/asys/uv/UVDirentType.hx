package asys.uv;

enum abstract UVDirentType(Int) {
  var DirentUnknown = 0;
  var DirentFile;
  var DirentDir;
  var DirentLink;
  var DirentFifo;
  var DirentSocket;
  var DirentChar;
  var DirentBlock;
}
