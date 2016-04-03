
#ifdef EPPC
#include <memory>
#else
#include <memory.h>
#endif


#ifndef HX_WINDOWS
#  include <strings.h>
#  undef strcmpi
#  define strcmpi(a,b) strcasecmp(a,b)
#else
#   include <string.h>
#endif


// -------------- parsing --------------------------


enum STATE {
   IGNORE_SPACES,
   BEGIN,
   BEGIN_NODE,
   TAG_NAME,
   BODY,
   ATTRIB_NAME,
   EQUALS,
   ATTVAL_BEGIN,
   ATTRIB_VAL,
   CHILDS,
   CLOSE,
   WAIT_END,
   WAIT_END_RET,
   PCDATA,
   HEADER,
   COMMENT,
   DOCTYPE,
   CDATA,
};

static void xml_error( const char *xml, const char *inWhere, int *line, String msg ) {
   String b = HX_CSTRING("Xml parse error : ") + msg + HX_CSTRING(" at line ") + String(*line) + HX_CSTRING(" : ");
   String where(inWhere);

   int l = where.length;
   int nchars = 30;
   if( inWhere != xml )
      b += HX_CSTRING("...");

   if (where.length==0)
      b+= HX_CSTRING("<eof>");
   else if (where.length<nchars)
      b+= where;
   else
      b+= where.substr(0,nchars) + HX_CSTRING("...");

   hx::Throw(b);
}

#define ERRORSTR(msg)   xml_error(xml,p,line,msg);
#define ERROR(msg)   xml_error(xml,p,line,HX_CSTRING(msg));

static bool is_valid_char( int c ) {
   return ( c >= 'a' && c <= 'z' ) || ( c >= 'A' && c <= 'Z' ) || ( c >= '0' && c <= '9' ) || c == ':' || c == '.' || c == '_' || c == '-';
}

static void do_parse_xml( const char *xml, const char **lp, int *line, cpp::NativeXmlState callb, String parentname )
{
   STATE state = BEGIN;
   STATE next = BEGIN;
   String aname;
   hx::Anon attribs;
   String nodename;

   const char *start = NULL;
   const char *p = *lp;
   char c = *p;
   int nsubs = 0, nbrackets = 0;
   while( c ) {
      switch( state ) {
      case IGNORE_SPACES:
         switch( c ) {
         case '\n':
         case '\r':
         case '\t':
         case ' ':
            break;
         default:
            state = next;
            continue;
         }
         break;
      case BEGIN:
         switch( c ) {
         case '<':
            state = IGNORE_SPACES;
            next = BEGIN_NODE;
            break;
         default:
            start = p;
            state = PCDATA;
            continue;
         }
         break;
      case PCDATA:
         if( c == '<' ) {
            callb->pcdata(String(start,p-start).dup());
            nsubs++;
            state = IGNORE_SPACES;
            next = BEGIN_NODE;
         }
         break;
      case CDATA:
         if( c == ']' && p[1] == ']' && p[2] == '>' ) {
            callb->cdata(String(start,p-start).dup());
            nsubs++;
            p += 2;
            state = BEGIN;
         }
         break;
      case BEGIN_NODE:
         switch( c ) {
         case '!':
            if( p[1] == '[' ) {
               p += 2;
               if( (p[0] != 'C' && p[0] != 'c') ||
                  (p[1] != 'D' && p[1] != 'd') ||
                  (p[2] != 'A' && p[2] != 'a') ||
                  (p[3] != 'T' && p[3] != 't') ||
                  (p[4] != 'A' && p[4] != 'a') ||
                  (p[5] != '[') )
                  ERROR("Expected <![CDATA[");
               p += 5;
               state = CDATA;
               start = p + 1;
               break;
            }
            if( p[1] == 'D' || p[1] == 'd' ) {
               if( (p[2] != 'O' && p[2] != 'o') ||
                  (p[3] != 'C' && p[3] != 'c') ||
                  (p[4] != 'T' && p[4] != 't') ||
                  (p[5] != 'Y' && p[5] != 'y') ||
                  (p[6] != 'P' && p[6] != 'p') ||
                  (p[7] != 'E' && p[7] != 'e') )
                  ERROR("Expected <!DOCTYPE");
               p += 7;
               state = DOCTYPE;
               start = p + 1;
               break;
            }
            if( p[1] != '-' || p[2] != '-' )
               ERROR("Expected <!--");
            p += 2;
            state = COMMENT;
            start = p + 1;
            break;
         case '?':
            state = HEADER;
            start = p;
            break;
         case '/':
            if( parentname.length==0 )
               ERROR("Expected node name");
            start = p + 1;
            state = IGNORE_SPACES;
            next = CLOSE;
            break;
         default:
            state = TAG_NAME;
            start = p;
            continue;
         }
         break;
      case TAG_NAME:
         if( !is_valid_char(c) ) {
            if( p == start )
               ERROR("Expected node name");
            nodename = String(start,p-start).dup();
            attribs = hx::Anon_obj::Create();
            state = IGNORE_SPACES;
            next = BODY;
            continue;
         }
         break;
      case BODY:
         switch( c ) {
         case '/':
            state = WAIT_END;
            nsubs++;
            callb->xml(nodename,attribs);
            break;
         case '>':
            state = CHILDS;
            nsubs++;
            callb->xml(nodename,attribs);
            break;
         default:
            state = ATTRIB_NAME;
            start = p;
            continue;
         }
         break;
      case ATTRIB_NAME:
         if( !is_valid_char(c) ) {
            if( start == p )
               ERROR("Expected attribute name");
            aname = String(start,p-start).dup();
            if( attribs->__Field(aname,hx::paccDynamic) != null() )
               ERROR("Duplicate attribute");
            state = IGNORE_SPACES;
            next = EQUALS;
            continue;
         }
         break;
      case EQUALS:
         switch( c ) {
         case '=':
            state = IGNORE_SPACES;
            next = ATTVAL_BEGIN;
            break;
         default:
            ERROR("Expected =");
         }
         break;
      case ATTVAL_BEGIN:
         switch( c ) {
         case '"':
         case '\'':
            state = ATTRIB_VAL;
            start = p;
            break;
         default:
            ERROR("Expected \"");
         }
         break;
      case ATTRIB_VAL:
         if( c == *start ) {
            attribs->Add( aname, String(start+1,p-start-1).dup() );
            state = IGNORE_SPACES;
            next = BODY;
         }
         break;
      case CHILDS:
         *lp = p;
         do_parse_xml(xml,lp,line,callb,nodename);
         p = *lp;
         start = p;
         state = BEGIN;
         break;
      case WAIT_END:
         switch( c ) {
         case '>':
            callb->done();
            state = BEGIN;
            break;
         default :
            ERROR("Expected >");
         }
         break;
      case WAIT_END_RET:
         switch( c ) {
         case '>':
            if( nsubs == 0 )
               callb->pcdata(HX_CSTRING(""));
            *lp = p;
            return;
         default :
            ERROR("Expected >");
         }
         break;
      case CLOSE:
         if( !is_valid_char(c) ) {
            if( start == p )
               ERROR("Expected node name");
            {
               String v = String(start,p - start).dup();
               if( strcmpi(parentname.__s,v.__s) != 0 ) {
                  ERRORSTR(HX_CSTRING("Expected </") + parentname + HX_CSTRING(">"));
               }
            }
            state = IGNORE_SPACES;
            next = WAIT_END_RET;
            continue;
         }
         break;
      case COMMENT:
         if( c == '-' && p[1] == '-' && p[2] == '>' ) {
            callb->comment(String(start,p-start).dup());
            p += 2;
            state = BEGIN;
         }
         break;
      case DOCTYPE:
         if( c == '[' )
            nbrackets++;
         else if( c == ']' )
            nbrackets--;
         else if( c == '>' && nbrackets == 0 ) {
            callb->doctype(String(start,p-start).dup());
            state = BEGIN;
         }
         break;
      case HEADER:
         if( c == '?' && p[1] == '>' ) {
            p++;
            callb->comment(String(start,p-start).dup());
            state = BEGIN;
         }
         break;
      }
      c = *++p;
      if( c == '\n' )
         (*line)++;
   }
   if( state == BEGIN ) {
      start = p;
      state = PCDATA;
   }
   if( parentname.__s == 0 && state == PCDATA ) {
      if( p != start || nsubs == 0 )
         callb->pcdata(String(start,p-start).dup());
      return;
   }
   ERROR("Unexpected end");
}

// ----------------------------------------------

/**
   <doc>
   <h1>Xml</h1>
   <p>
   The standard event-driven XML parser.
   </p>
   </doc>
**/

/**
   parse_xml : xml:string -> events:object -> void
   <doc>
   The [parse_xml] parse a string and for each parsed element call the
   corresponding object method in [events] :
   <ul>
   <li>[void xml( name : string, attribs : object)] when an XML node is found</li>
   <li>[void done()] when an XML node is closed</li>
   <li>[void pcdata(string)] when PCData chars found</li>
   <li>[void cdata(string)] when a CData session is found</li>
   <li>[void comment(string)] when some comment or special header is found</li>
   </ul>
   You can then implement the events so they build the appropriate XML data
   structure needed by your language.
   </doc>
**/
static void parse_xml( String str, cpp::NativeXmlState state )
{
   int line = 0;
   const char *p = str.__s;
   // skip BOM
   if( p[0] == (char)0xEF && p[1] == (char)0xBB && p[2] == (char)0xBF )
      p += 3;
   do_parse_xml(p,&p,&line,state,String());
}


