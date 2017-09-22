package haxe.web;

/**
 * Html MimeType Enum
 * <br> see : http://www.sitepoint.com/web-foundations/mime-types-complete-list/
 * @class MimeType
 * @enum
 */

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


/**
 * @property CCAD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CCO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CDF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property X_CDF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NET_CDF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CER
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property X_CER
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CHA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CHAT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CLASS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property BYTE_CLASS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property X_CLASS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property COM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CONF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CPIO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property X_CPP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CPT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CRL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CRT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CSH
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CSS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property CXX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DCR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DEEPV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DEF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DER
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DIR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DOC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DOT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DRW
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DUMP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DVI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DWF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DWG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DXF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property DXR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property EL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ELC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ENV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property EPS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ES
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ETX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property EVY
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property EXE
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property F
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property F77
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property F90
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FDF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FLI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FLO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FLX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FMF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FOR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FPX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FRL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property FUNK
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property G
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property G3
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property GIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property GL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property GSD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property GSM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property GSP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property GSS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property GTAR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property GZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property GZIP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property H
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HDF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HELP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HGL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HH
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HLB
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HLP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HPG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HPGL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HQX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HTA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HTC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HTM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HTML
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HTMLS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HTT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property HTX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ICE
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ICO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IDC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IEF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IEFS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IGES
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IGS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IMA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IMAP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property INF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property INS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ISU
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IVR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property IVY
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JAM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JAV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JAVA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JCM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JFIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JFIF_TBNL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JPE
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JPEG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JPG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JPS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property JUT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property KAR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property KSH
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LAM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LATEX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LHA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LHX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LIST
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LMA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LOG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LSP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LST
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LSX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LTX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LZH
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property LZX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property M
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property M1V
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property M2A
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property M2V
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property M3U
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MAN
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MAP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MAR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MBD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MC_DOLLAR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MCD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MCF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MCP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ME
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MHT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MHTML
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MID
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MIDI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MIME
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MJF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MJPG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MME
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MOD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MOOV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MOVIE
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MP2
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MP3
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPE
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPEG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPGA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MPX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MRC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MY
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property MZZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NAP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NAPLPS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NCM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NIFF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NIX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NSC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property NVD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property O
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ODA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property OMC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property OMCD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property OMCR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property P
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property P10
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property P12
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property P7A
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property P7C
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property P7M
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property P7R
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property P7S
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PART
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PAS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PBM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PCL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PCT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PCX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PDB
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PDF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PFUNK
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PGM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PIC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PICT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PKG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PKO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PLX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PM4
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PM5
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PNG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PNM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property POT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property POV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PPA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PPM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PPS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PPT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PPZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PRE
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PRT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PSD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PVU
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PWZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PY
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property PYC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property QCP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property QD3
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property QD3D
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property QIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property QT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property QTC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property QTI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property QTIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RAM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RAS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RAST
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property REXX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RGB
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RMI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RMM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RMP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RNG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RNX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ROFF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RPM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RTF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RTX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property RV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property S
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property S3M
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SAVEME
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SBK
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SCM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SDML
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SDP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SDR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SEA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SET
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SGM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SGML
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SH
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SHAR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SHTML
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SID
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SIT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SKD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SKM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SKP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SKT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SMI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SMIL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SND
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SOL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SPC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SPL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SPR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SPRITE
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SRC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SSI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SSM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SST
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property STEP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property STL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property STP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SV4CPIO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SV4CRC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SVF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SVR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property SWF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property T
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TALK
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TAR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TBK
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TCL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TCSH
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TEX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TEXI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TEXINFO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TEXT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TGZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TIFF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TSI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TSP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TSV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TURBOT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property TXT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property UIL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property UNI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property UNIS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property UNV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property URI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property URIS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property USTAR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property UU
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property UUE
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VCD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VCS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VDA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VDO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VEW
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VIV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VIVO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VMD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VMF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VOC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VOS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VOX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VQE
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VQF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VQL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VRML
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VRT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VSD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VST
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property VSW
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property W60
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property W61
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property W6W
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WAV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WB1
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WBMP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WEB
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WEBP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WIZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WK1
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WMF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WML
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WMLC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WMLS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WMLSC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WORD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WP5
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WP6
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WPD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WQ1
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WRI
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WRL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WRZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WSC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WSRC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property WTK
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XBM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XDR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XGZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XIF
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLA
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLB
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLC
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLK
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLL
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLS
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLT
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLV
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XLW
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XML
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XMZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XPIX
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XPM
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property X_PNG
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XSR
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XWD
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property XYZ
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property Z
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ZIP
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ZOO
 * @type MimeType
 * @static
 * @readOnly
 */
/**
 * @property ZSH
 * @type MimeType
 * @static
 * @readOnly
 */
