package haxe.io;

/**
    Html MimeType Enum
    @see : http://www.sitepoint.com/web-foundations/mime-types-complete-list/
 **/

@:enum abstract MIME(String) from String to String {
    var _3DM = 'x-world/x-3dmf';
    var _3DMF = 'x-world/x-3dmf';
    var A = 'application/octet-stream';
    var AAB = 'application/x-authorware-bin';
    var AAM = 'application/x-authorware-map';
    var AAS = 'application/x-authorware-seg';
    var ABC = 'text/vnd.abc';
    var ACGI = 'text/html';
    var AFL = 'video/animaflex';
    var AI = 'application/postscript';
    var AIF = 'audio/aiff';
    var X_AIF = 'audio/x-aiff';
    var AIFC = 'audio/aiff';
    var X_AIFC = 'audio/x-aiff';
    var AIFF = 'audio/aiff';
    var X_AIFF = 'audio/x-aiff';
    var AIM = 'application/x-aim';
    var AIP = 'text/x-audiosoft-intra';
    var ANI = 'application/x-navi-animation';
    var AOS = 'application/x-nokia-9000-communicator-add-on-software';
    var APS = 'application/mime';
    var ARC = 'application/octet-stream';
    var ARJ = 'application/arj';
    var ART = 'image/x-jg';
    var ASF = 'video/x-ms-asf';
    var ASM = 'text/x-asm';
    var ASP = 'text/asp';
    var ASX = 'application/x-mplayer2';
    var AU = 'audio/basic';
    var X_AVI = 'application/x-troff-msvideo';
    var AVI = 'video/avi';
    var MS_AVI = 'video/msvideo';
    var X_MS_AVI = 'video/x-msvideo';
    var AVS = 'video/avs-video';
    var BCPIO = 'application/x-bcpio';
    var MAC_BIN = 'application/mac-binary';
    var MC_BIN = 'application/macbinary';
    var BIN = 'application/octet-stream';
    var X_BIN = 'application/x-binary';
    var X_MAC_BIN = 'application/x-macbinary';
    var BM = 'image/bmp';
    var BMP = 'image/bmp';
    var X_BMP = 'image/x-windows-bmp';
    var BOO = 'application/book';
    var BOOK = 'application/book';
    var BOZ = 'application/x-bzip2';
    var BSH = 'application/x-bsh';
    var BZ = 'application/x-bzip';
    var BZ2 = 'application/x-bzip2';
    var C = 'text/plain';
    var X_C = 'text/x-c';
    var CPP = 'text/plain';
    var CAT = 'application/vnd.ms-pki.seccat';
    var CC = 'text/plain';
    var X_CC = 'text/x-c';
    var CCAD = 'application/clariscad';
    var CCO = 'application/x-cocoa';
    var CDF = 'application/cdf';
    var X_CDF = 'application/x-cdf';
    var NET_CDF = 'application/x-netcdf';
    var CER = 'application/pkix-cert';
    var X_CER = 'application/x-x509-ca-cert';
    var CHA = 'application/x-chat';
    var CHAT = 'application/x-chat';
    var CLASS = 'application/java';
    var BYTE_CLASS = 'application/java-byte-code';
    var X_CLASS = 'application/x-java-class';
    var COM = 'application/octet-stream';
    var CONF = 'text/plain';
    var CPIO = 'application/x-cpio';
    var X_CPP = 'text/x-c';
    var CPT = 'application/mac-compactpro';
    var CRL = 'application/pkcs-crl';
    var CRT = 'application/pkix-cert';
    var CSH = 'application/x-csh';
    var CSS = 'text/css';
    var CXX = 'text/plain';
    var DCR = 'application/x-director';
    var DEEPV = 'application/x-deepv';
    var DEF = 'text/plain';
    var DER = 'application/x-x509-ca-cert';
    var DIF = 'video/x-dv';
    var DIR = 'application/x-director';
    var DL = 'video/dl';
    var DOC = 'application/msword';
    var DOT = 'application/msword';
    var DP = 'application/commonground';
    var DRW = 'application/drafting';
    var DUMP = 'application/octet-stream';
    var DV = 'video/x-dv';
    var DVI = 'application/x-dvi';
    var DWF = 'drawing/x-dwf (old)';
    var DWG = 'application/acad';
    var DXF = 'application/dxf';
    var DXR = 'application/x-director';
    var EL = 'text/x-script.elisp';
    var ELC = 'application/x-bytecode.elisp (compiled elisp)';
    var ENV = 'application/x-envoy';
    var EPS = 'application/postscript';
    var ES = 'application/x-esrehber';
    var ETX = 'text/x-setext';
    var EVY = 'application/envoy';
    var EXE = 'application/octet-stream';
    var F = 'text/plain';
    var F77 = 'text/x-fortran';
    var F90 = 'text/plain';
    var FDF = 'application/vnd.fdf';
    var FIF = 'image/fif';
    var FLI = 'video/fli';
    var FLO = 'image/florian';
    var FLX = 'text/vnd.fmi.flexstor';
    var FMF = 'video/x-atomic3d-feature';
    var FOR = 'text/plain';
    var FPX = 'image/vnd.fpx';
    var FRL = 'application/freeloader';
    var FUNK = 'audio/make';
    var G = 'text/plain';
    var G3 = 'image/g3fax';
    var GIF = 'image/gif';
    var GL = 'video/gl';
    var GSD = 'audio/x-gsm';
    var GSM = 'audio/x-gsm';
    var GSP = 'application/x-gsp';
    var GSS = 'application/x-gss';
    var GTAR = 'application/x-gtar';
    var GZ = 'application/x-compressed';
    var GZIP = 'application/x-gzip';
    var H = 'text/plain';
    var HDF = 'application/x-hdf';
    var HELP = 'application/x-helpfile';
    var HGL = 'application/vnd.hp-hpgl';
    var HH = 'text/plain';
    var HLB = 'text/x-script';
    var HLP = 'application/hlp';
    var HPG = 'application/vnd.hp-hpgl';
    var HPGL = 'application/vnd.hp-hpgl';
    var HQX = 'application/binhex';
    var HTA = 'application/hta';
    var HTC = 'text/x-component';
    var HTM = 'text/html';
    var HTML = 'text/html';
    var HTMLS = 'text/html';
    var HTT = 'text/webviewhtml';
    var HTX = 'text/html';
    var ICE = 'x-conference/x-cooltalk';
    var ICO = 'image/x-icon';
    var IDC = 'text/plain';
    var IEF = 'image/ief';
    var IEFS = 'image/ief';
    var IGES = 'application/iges';
    var IGS = 'application/iges';
    var IMA = 'application/x-ima';
    var IMAP = 'application/x-httpd-imap';
    var INF = 'application/inf';
    var INS = 'application/x-internett-signup';
    var IP = 'application/x-ip2';
    var ISU = 'video/x-isvideo';
    var IT = 'audio/it';
    var IV = 'application/x-inventor';
    var IVR = 'i-world/i-vrml';
    var IVY = 'application/x-livescreen';
    var JAM = 'audio/x-jam';
    var JAV = 'text/plain';
    var JAVA = 'text/plain';
    var JCM = 'application/x-java-commerce';
    var JFIF = 'image/jpeg';
    var JFIF_TBNL = 'image/jpeg';
    var JPE = 'image/jpeg';
    var JPEG = 'image/jpeg';
    var JPG = 'image/jpeg';
    var JPS = 'image/x-jps';
    var JS = 'text/javascript';
    var JSON = 'application/json';
    var JSONP = 'application/javascript';
    var JUT = 'image/jutvision';
    var KAR = 'audio/midi';
    var KSH = 'application/x-ksh';
    var LA = 'audio/nspaudio';
    var LAM = 'audio/x-liveaudio';
    var LATEX = 'application/x-latex';
    var LHA = 'application/octet-stream';
    var LHX = 'application/octet-stream';
    var LIST = 'text/plain';
    var LMA = 'audio/nspaudio';
    var LOG = 'text/plain';
    var LSP = 'application/x-lisp';
    var LST = 'text/plain';
    var LSX = 'text/x-la-asf';
    var LTX = 'application/x-latex';
    var LZH = 'application/octet-stream';
    var LZX = 'application/lzx';
    var M = 'text/plain';
    var M1V = 'video/mpeg';
    var M2A = 'audio/mpeg';
    var M2V = 'video/mpeg';
    var M3U = 'audio/x-mpequrl';
    var MAN = 'application/x-troff-man';
    var MAP = 'application/x-navimap';
    var MAR = 'text/plain';
    var MBD = 'application/mbedlet';
    var MC_DOLLAR = 'application/x-magic-cap-package-1.0';
    var MCD = 'application/mcad';
    var MCF = 'image/vasa';
    var MCP = 'application/netmc';
    var ME = 'application/x-troff-me';
    var MHT = 'message/rfc822';
    var MHTML = 'message/rfc822';
    var MID = 'audio/midi';
    var MIDI = 'audio/midi';
    var MIF = 'application/x-mif';
    var MIME = 'www/mime';
    var MJF = 'audio/x-vnd.audioexplosion.mjuicemediafile';
    var MJPG = 'video/x-motion-jpeg';
    var MM = 'application/base64';
    var MME = 'application/base64';
    var MOD = 'audio/mod';
    var MOOV = 'video/quicktime';
    var MOVIE = 'video/x-sgi-movie';
    var MP2 = 'video/mpeg';
    var MP3 = 'audio/mpeg3';
    var MPA = 'audio/mpeg';
    var MPC = 'application/x-project';
    var MPE = 'video/mpeg';
    var MPEG = 'video/mpeg';
    var MPG = 'video/mpeg';
    var MPGA = 'audio/mpeg';
    var MPP = 'application/vnd.ms-project';
    var MPT = 'application/x-project';
    var MPV = 'application/x-project';
    var MPX = 'application/x-project';
    var MRC = 'application/marc';
    var MS = 'application/x-troff-ms';
    var MV = 'video/x-sgi-movie';
    var MY = 'audio/make';
    var MZZ = 'application/x-vnd.audioexplosion.mzz';
    var NAP = 'image/naplps';
    var NAPLPS = 'image/naplps';
    var NC = 'application/x-netcdf';
    var NCM = 'application/vnd.nokia.configuration-message';
    var NIF = 'image/x-niff';
    var NIFF = 'image/x-niff';
    var NIX = 'application/x-mix-transfer';
    var NSC = 'application/x-conference';
    var NVD = 'application/x-navidoc';
    var O = 'application/octet-stream';
    var ODA = 'application/oda';
    var OMC = 'application/x-omc';
    var OMCD = 'application/x-omcdatamaker';
    var OMCR = 'application/x-omcregerator';
    var P = 'text/x-pascal';
    var P10 = 'application/pkcs10';
    var P12 = 'application/pkcs-12';
    var P7A = 'application/x-pkcs7-signature';
    var P7C = 'application/pkcs7-mime';
    var P7M = 'application/pkcs7-mime';
    var P7R = 'application/x-pkcs7-certreqresp';
    var P7S = 'application/pkcs7-signature';
    var PART = 'application/pro_eng';
    var PAS = 'text/pascal';
    var PBM = 'image/x-portable-bitmap';
    var PCL = 'application/vnd.hp-pcl';
    var PCT = 'image/x-pict';
    var PCX = 'image/x-pcx';
    var PDB = 'chemical/x-pdb';
    var PDF = 'application/pdf';
    var PFUNK = 'audio/make';
    var PGM = 'image/x-portable-graymap';
    var PIC = 'image/pict';
    var PICT = 'image/pict';
    var PKG = 'application/x-newton-compatible-pkg';
    var PKO = 'application/vnd.ms-pki.pko';
    var PL = 'text/plain';
    var PLX = 'application/x-pixclscript';
    var PM = 'image/x-xpixmap';
    var PM4 = 'application/x-pagemaker';
    var PM5 = 'application/x-pagemaker';
    var PNG = 'image/png';
    var PNM = 'application/x-portable-anymap';
    var POT = 'application/mspowerpoint';
    var POV = 'model/x-pov';
    var PPA = 'application/vnd.ms-powerpoint';
    var PPM = 'image/x-portable-pixmap';
    var PPS = 'application/mspowerpoint';
    var PPT = 'application/mspowerpoint';
    var PPZ = 'application/mspowerpoint';
    var PRE = 'application/x-freelance';
    var PRT = 'application/pro_eng';
    var PS = 'application/postscript';
    var PSD = 'application/octet-stream';
    var PVU = 'paleovu/x-pv';
    var PWZ = 'application/vnd.ms-powerpoint';
    var PY = 'text/x-script.phyton';
    var PYC = 'application/x-bytecode.python';
    var QCP = 'audio/vnd.qcelp';
    var QD3 = 'x-world/x-3dmf';
    var QD3D = 'x-world/x-3dmf';
    var QIF = 'image/x-quicktime';
    var QT = 'video/quicktime';
    var QTC = 'video/x-qtc';
    var QTI = 'image/x-quicktime';
    var QTIF = 'image/x-quicktime';
    var RA = 'audio/x-pn-realaudio';
    var RAM = 'audio/x-pn-realaudio';
    var RAS = 'application/x-cmu-raster';
    var RAST = 'image/cmu-raster';
    var REXX = 'text/x-script.rexx';
    var RF = 'image/vnd.rn-realflash';
    var RGB = 'image/x-rgb';
    var RM = 'application/vnd.rn-realmedia';
    var RMI = 'audio/mid';
    var RMM = 'audio/x-pn-realaudio';
    var RMP = 'audio/x-pn-realaudio';
    var RNG = 'application/ringing-tones';
    var RNX = 'application/vnd.rn-realplayer';
    var ROFF = 'application/x-troff';
    var RP = 'image/vnd.rn-realpix';
    var RPM = 'audio/x-pn-realaudio-plugin';
    var RT = 'text/richtext';
    var RTF = 'application/rtf';
    var RTX = 'application/rtf';
    var RV = 'video/vnd.rn-realvideo';
    var S = 'text/x-asm';
    var S3M = 'audio/s3m';
    var SAVEME = 'application/octet-stream';
    var SBK = 'application/x-tbook';
    var SCM = 'application/x-lotusscreencam';
    var SDML = 'text/plain';
    var SDP = 'application/sdp';
    var SDR = 'application/sounder';
    var SEA = 'application/sea';
    var SET = 'application/set';
    var SGM = 'text/sgml';
    var SGML = 'text/sgml';
    var SH = 'application/x-bsh';
    var SHAR = 'application/x-bsh';
    var SHTML = 'text/html';
    var SID = 'audio/x-psid';
    var SIT = 'application/x-sit';
    var SKD = 'application/x-koan';
    var SKM = 'application/x-koan';
    var SKP = 'application/x-koan';
    var SKT = 'application/x-koan';
    var SL = 'application/x-seelogo';
    var SMI = 'application/smil';
    var SMIL = 'application/smil';
    var SND = 'audio/basic';
    var SOL = 'application/solids';
    var SPC = 'application/x-pkcs7-certificates';
    var SPL = 'application/futuresplash';
    var SPR = 'application/x-sprite';
    var SPRITE = 'application/x-sprite';
    var SRC = 'application/x-wais-source';
    var SSI = 'text/x-server-parsed-html';
    var SSM = 'application/streamingmedia';
    var SST = 'application/vnd.ms-pki.certstore';
    var STEP = 'application/step';
    var STL = 'application/sla';
    var STP = 'application/step';
    var SV4CPIO = 'application/x-sv4cpio';
    var SV4CRC = 'application/x-sv4crc';
    var SVF = 'image/vnd.dwg';
    var SVR = 'application/x-world';
    var SWF = 'application/x-shockwave-flash';
    var T = 'application/x-troff';
    var TALK = 'text/x-speech';
    var TAR = 'application/x-tar';
    var TBK = 'application/toolbook';
    var TCL = 'application/x-tcl';
    var TCSH = 'text/x-script.tcsh';
    var TEX = 'application/x-tex';
    var TEXI = 'application/x-texinfo';
    var TEXINFO = 'application/x-texinfo';
    var TEXT = 'text/plain';
    var TGZ = 'application/gnutar';
    var TIF = 'image/tiff';
    var TIFF = 'image/tiff';
    var TR = 'application/x-troff';
    var TSI = 'audio/tsp-audio';
    var TSP = 'application/dsptype';
    var TSV = 'text/tab-separated-values';
    var TURBOT = 'image/florian';
    var TXT = 'text/plain';
    var UIL = 'text/x-uil';
    var UNI = 'text/uri-list';
    var UNIS = 'text/uri-list';
    var UNV = 'application/i-deas';
    var URI = 'text/uri-list';
    var URIS = 'text/uri-list';
    var USTAR = 'application/x-ustar';
    var UU = 'application/octet-stream';
    var UUE = 'text/x-uuencode';
    var VCD = 'application/x-cdlink';
    var VCS = 'text/x-vcalendar';
    var VDA = 'application/vda';
    var VDO = 'video/vdo';
    var VEW = 'application/groupwise';
    var VIV = 'video/vivo';
    var VIVO = 'video/vivo';
    var VMD = 'application/vocaltec-media-desc';
    var VMF = 'application/vocaltec-media-file';
    var VOC = 'audio/voc';
    var VOS = 'video/vosaic';
    var VOX = 'audio/voxware';
    var VQE = 'audio/x-twinvq-plugin';
    var VQF = 'audio/x-twinvq';
    var VQL = 'audio/x-twinvq-plugin';
    var VRML = 'application/x-vrml';
    var VRT = 'x-world/x-vrt';
    var VSD = 'application/x-visio';
    var VST = 'application/x-visio';
    var VSW = 'application/x-visio';
    var W60 = 'application/wordperfect6.0';
    var W61 = 'application/wordperfect6.1';
    var W6W = 'application/msword';
    var WAV = 'audio/wav';
    var WB1 = 'application/x-qpro';
    var WBMP = 'image/vnd.wap.wbmp';
    var WEB = 'application/vnd.xara';
    var WEBP = 'image/webp';
    var WIZ = 'application/msword';
    var WK1 = 'application/x-123';
    var WMF = 'windows/metafile';
    var WML = 'text/vnd.wap.wml';
    var WMLC = 'application/vnd.wap.wmlc';
    var WMLS = 'text/vnd.wap.wmlscript';
    var WMLSC = 'application/vnd.wap.wmlscriptc';
    var WORD = 'application/msword';
    var WP = 'application/wordperfect';
    var WP5 = 'application/wordperfect';
    var WP6 = 'application/wordperfect';
    var WPD = 'application/wordperfect';
    var WQ1 = 'application/x-lotus';
    var WRI = 'application/mswrite';
    var WRL = 'application/x-world';
    var WRZ = 'model/vrml';
    var WSC = 'text/scriplet';
    var WSRC = 'application/x-wais-source';
    var WTK = 'application/x-wintalk';
    var XBM = 'image/x-xbitmap';
    var XDR = 'video/x-amt-demorun';
    var XGZ = 'xgl/drawing';
    var XIF = 'image/vnd.xiff';
    var XL = 'application/excel';
    var XLA = 'application/excel';
    var XLB = 'application/excel';
    var XLC = 'application/excel';
    var XLD = 'application/excel';
    var XLK = 'application/excel';
    var XLL = 'application/excel';
    var XLM = 'application/excel';
    var XLS = 'application/excel';
    var XLT = 'application/excel';
    var XLV = 'application/excel';
    var XLW = 'application/excel';
    var XM = 'audio/xm';
    var XML = 'application/xml';
    var XMZ = 'xgl/movie';
    var XPIX = 'application/x-vnd.ls-xpix';
    var XPM = 'image/x-xpixmap';
    var X_PNG = 'image/png';
    var XSR = 'video/x-amt-showrun';
    var XWD = 'image/x-xwd';
    var XYZ = 'chemical/x-pdb';
    var Z = 'application/x-compress';
    var ZIP = 'multipart/x-zip';
    var ZOO = 'application/octet-stream';
    var ZSH = 'text/x-script.zsh';
}
