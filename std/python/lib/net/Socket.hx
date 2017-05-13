/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package python.lib.net;

import haxe.io.BytesData;
import python.Tuple;

/**
    A TCP socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/

@:pythonImport("socket", "socket")
extern class Socket {
    function send(d:BytesData,flags:Int):Int;
    function recv(n:Int,flags:Int):BytesData;

    /**
        Creates a new unconnected socket.
    **/
    function new() : Void;

    /**
        Closes the socket : make sure to properly close all your sockets or you will crash when you run out of file descriptors.
    **/
    function close() : Void;

    /**
        Connect to the given server host/port. Throw an exception in case we couldn't successfully connect.
    **/
    function connect( addr : python.lib.net.Address ) : Void;

    //function create_connection() :

    /**
        Allow the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused. Use `accept()` to accept incoming connections.
    **/
    function listen( connections : Int ) : Void;

    /**
        Shutdown the socket, either for reading or writing.
    **/
    function shutdown( how :Int ) : Void;

    /**
        Bind the socket to the given host/port so it can afterwards listen for connections there.
    **/
    function bind( address : python.lib.net.Address ) : Void;

    /**
        Accept a new connected client. This will return a connected socket on which you can read/write some data.
    **/
    function accept() : Tuple2<Socket,Address>;

    /**
        Return the information about the other side of a connected socket.
    **/
    function getpeername() : python.lib.net.Address;

    /**
        Return the information about our side of a connected socket.
    **/
    function getsockname() : python.lib.net.Address;

    /**
        Gives a timeout after which blocking socket operations (such as reading and writing) will abort and throw an exception.
    **/
    function settimeout( timeout : Float ) : Void;

    /**
        Block until some data is available for read on the socket.
    **/
    function waitForRead() : Void;

    /**
        Change the blocking mode of the socket. A blocking socket is the default behavior. A non-blocking socket will abort blocking operations immediately by throwing a haxe.io.Error.Blocking value.
    **/
    function setblocking( b : Bool ) : Void;

    /**

    **/
    function setsockopt( family:Int, option:Int, value : Bool ) : Void;


    function fileno():Int;

    /**
        Wait until one of the sockets groups is ready for the given operation :
        - `read` contains sockets on which we want to wait for available data to be read,
        - `write` contains sockets on which we want to wait until we are allowed to write some data to their output buffers,
        - `others` contains sockets on which we want to wait for exceptional conditions.
        - `select` will block until one of the condition is met, in which case it will return the sockets for which the condition was true.
        In case a `timeout` (in seconds) is specified, select might wait at worse until the timeout expires.
    **/
    //static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> };

}

@:pythonImport("socket")
extern class SocketModule {
    static var AF_APPLETALK:Int;
    static var AF_ASH:Int;
    static var AF_ATMPVC:Int;
    static var AF_ATMSVC:Int;
    static var AF_AX25:Int;
    static var AF_BLUETOOTH:Int;
    static var AF_BRIDGE:Int;
    static var AF_CAN:Int;
    static var AF_ECONET:Int;
    static var AF_INET:Int;
    static var AF_INET6:Int;
    static var AF_IPX:Int;
    static var AF_IRDA:Int;
    static var AF_KEY:Int;
    static var AF_LLC:Int;
    static var AF_NETBEUI:Int;
    static var AF_NETLINK:Int;
    static var AF_NETROM:Int;
    static var AF_PACKET:Int;
    static var AF_PPPOX:Int;
    static var AF_RDS:Int;
    static var AF_ROSE:Int;
    static var AF_ROUTE:Int;
    static var AF_SECURITY:Int;
    static var AF_SNA:Int;
    static var AF_TIPC:Int;
    static var AF_UNIX:Int;
    static var AF_UNSPEC:Int;
    static var AF_WANPIPE:Int;
    static var AF_X25:Int;
    static var AI_ADDRCONFIG:Int;
    static var AI_ALL:Int;
    static var AI_CANONNAME:Int;
    static var AI_NUMERICHOST:Int;
    static var AI_NUMERICSERV:Int;
    static var AI_PASSIVE:Int;
    static var AI_V4MAPPED:Int;
    static var BDADDR_ANY:Int;
    static var BDADDR_LOCAL:Int;
    static var BTPROTO_HCI:Int;
    static var BTPROTO_L2CAP:Int;
    static var BTPROTO_RFCOMM:Int;
    static var BTPROTO_SCO:Int;
    static var CAN_EFF_FLAG:Int;
    static var CAN_EFF_MASK:Int;
    static var CAN_ERR_FLAG:Int;
    static var CAN_ERR_MASK:Int;
    static var CAN_RAW:Int;
    static var CAN_RAW_ERR_FILTER:Int;
    static var CAN_RAW_FILTER:Int;
    static var CAN_RAW_LOOPBACK:Int;
    static var CAN_RAW_RECV_OWN_MSGS:Int;
    static var CAN_RTR_FLAG:Int;
    static var CAN_SFF_MASK:Int;
    static var CAPI:Int;
    static var CMSG_LEN:Int;
    static var CMSG_SPACE:Int;
    static var EAGAIN:Int;
    static var EAI_ADDRFAMILY:Int;
    static var EAI_AGAIN:Int;
    static var EAI_BADFLAGS:Int;
    static var EAI_FAIL:Int;
    static var EAI_FAMILY:Int;
    static var EAI_MEMORY:Int;
    static var EAI_NODATA:Int;
    static var EAI_NONAME:Int;
    static var EAI_OVERFLOW:Int;
    static var EAI_SERVICE:Int;
    static var EAI_SOCKTYPE:Int;
    static var EAI_SYSTEM:Int;
    static var EBADF:Int;
    static var EWOULDBLOCK:Int;
    static var HCI_DATA_DIR:Int;
    static var HCI_FILTER:Int;
    static var HCI_TIME_STAMP:Int;
    static var INADDR_ALLHOSTS_GROUP:Int;
    static var INADDR_ANY:Int;
    static var INADDR_BROADCAST:Int;
    static var INADDR_LOOPBACK:Int;
    static var INADDR_MAX_LOCAL_GROUP:Int;
    static var INADDR_NONE:Int;
    static var INADDR_UNSPEC_GROUP:Int;
    static var IPPORT_RESERVED:Int;
    static var IPPORT_USERRESERVED:Int;
    static var IPPROTO_AH:Int;
    static var IPPROTO_DSTOPTS:Int;
    static var IPPROTO_EGP:Int;
    static var IPPROTO_ESP:Int;
    static var IPPROTO_FRAGMENT:Int;
    static var IPPROTO_GRE:Int;
    static var IPPROTO_HOPOPTS:Int;
    static var IPPROTO_ICMP:Int;
    static var IPPROTO_ICMPV6:Int;
    static var IPPROTO_IDP:Int;
    static var IPPROTO_IGMP:Int;
    static var IPPROTO_IP:Int;
    static var IPPROTO_IPIP:Int;
    static var IPPROTO_IPV6:Int;
    static var IPPROTO_NONE:Int;
    static var IPPROTO_PIM:Int;
    static var IPPROTO_PUP:Int;
    static var IPPROTO_RAW:Int;
    static var IPPROTO_ROUTING:Int;
    static var IPPROTO_RSVP:Int;
    static var IPPROTO_SCTP:Int;
    static var IPPROTO_TCP:Int;
    static var IPPROTO_TP:Int;
    static var IPPROTO_UDP:Int;
    static var IPV6_CHECKSUM:Int;
    static var IPV6_DSTOPTS:Int;
    static var IPV6_HOPLIMIT:Int;
    static var IPV6_HOPOPTS:Int;
    static var IPV6_JOIN_GROUP:Int;
    static var IPV6_LEAVE_GROUP:Int;
    static var IPV6_MULTICAST_HOPS:Int;
    static var IPV6_MULTICAST_IF:Int;
    static var IPV6_MULTICAST_LOOP:Int;
    static var IPV6_NEXTHOP:Int;
    static var IPV6_PKTINFO:Int;
    static var IPV6_RECVDSTOPTS:Int;
    static var IPV6_RECVHOPLIMIT:Int;
    static var IPV6_RECVHOPOPTS:Int;
    static var IPV6_RECVPKTINFO:Int;
    static var IPV6_RECVRTHDR:Int;
    static var IPV6_RECVTCLASS:Int;
    static var IPV6_RTHDR:Int;
    static var IPV6_RTHDRDSTOPTS:Int;
    static var IPV6_RTHDR_TYPE_0:Int;
    static var IPV6_TCLASS:Int;
    static var IPV6_UNICAST_HOPS:Int;
    static var IPV6_V6ONLY:Int;
    static var IP_ADD_MEMBERSHIP:Int;
    static var IP_DEFAULT_MULTICAST_LOOP:Int;
    static var IP_DEFAULT_MULTICAST_TTL:Int;
    static var IP_DROP_MEMBERSHIP:Int;
    static var IP_HDRINCL:Int;
    static var IP_MAX_MEMBERSHIPS:Int;
    static var IP_MULTICAST_IF:Int;
    static var IP_MULTICAST_LOOP:Int;
    static var IP_MULTICAST_TTL:Int;
    static var IP_OPTIONS:Int;
    static var IP_RECVOPTS:Int;
    static var IP_RECVRETOPTS:Int;
    static var IP_RETOPTS:Int;
    static var IP_TOS:Int;
    static var IP_TRANSPARENT:Int;
    static var IP_TTL:Int;
    static var MSG_CMSG_CLOEXEC:Int;
    static var MSG_CONFIRM:Int;
    static var MSG_CTRUNC:Int;
    static var MSG_DONTROUTE:Int;
    static var MSG_DONTWAIT:Int;
    static var MSG_EOR:Int;
    static var MSG_ERRQUEUE:Int;
    static var MSG_MORE:Int;
    static var MSG_NOSIGNAL:Int;
    static var MSG_OOB:Int;
    static var MSG_PEEK:Int;
    static var MSG_TRUNC:Int;
    static var MSG_WAITALL:Int;
    static var NETLINK_DNRTMSG:Int;
    static var NETLINK_FIREWALL:Int;
    static var NETLINK_IP6_FW:Int;
    static var NETLINK_NFLOG:Int;
    static var NETLINK_ROUTE:Int;
    static var NETLINK_USERSOCK:Int;
    static var NETLINK_XFRM:Int;
    static var NI_DGRAM:Int;
    static var NI_MAXHOST:Int;
    static var NI_MAXSERV:Int;
    static var NI_NAMEREQD:Int;
    static var NI_NOFQDN:Int;
    static var NI_NUMERICHOST:Int;
    static var NI_NUMERICSERV:Int;
    static var PACKET_BROADCAST:Int;
    static var PACKET_FASTROUTE:Int;
    static var PACKET_HOST:Int;
    static var PACKET_LOOPBACK:Int;
    static var PACKET_MULTICAST:Int;
    static var PACKET_OTHERHOST:Int;
    static var PACKET_OUTGOING:Int;
    static var PF_CAN:Int;
    static var PF_PACKET:Int;
    static var PF_RDS:Int;
    static var SCM_CREDENTIALS:Int;
    static var SCM_RIGHTS:Int;
    static var SHUT_RD:Int;
    static var SHUT_RDWR:Int;
    static var SHUT_WR:Int;
    static var SOCK_CLOEXEC:Int;
    static var SOCK_DGRAM:Int;
    static var SOCK_NONBLOCK:Int;
    static var SOCK_RAW:Int;
    static var SOCK_RDM:Int;
    static var SOCK_SEQPACKET:Int;
    static var SOCK_STREAM:Int;
    static var SOL_CAN_BASE:Int;
    static var SOL_CAN_RAW:Int;
    static var SOL_HCI:Int;
    static var SOL_IP:Int;
    static var SOL_SOCKET:Int;
    static var SOL_TCP:Int;
    static var SOL_TIPC:Int;
    static var SOL_UDP:Int;
    static var SOMAXCONN:Int;
    static var SO_ACCEPTCONN:Int;
    static var SO_BINDTODEVICE:Int;
    static var SO_BROADCAST:Int;
    static var SO_DEBUG:Int;
    static var SO_DONTROUTE:Int;
    static var SO_ERROR:Int;
    static var SO_KEEPALIVE:Int;
    static var SO_LINGER:Int;
    static var SO_OOBINLINE:Int;
    static var SO_PASSCRED:Int;
    static var SO_PEERCRED:Int;
    static var SO_RCVBUF:Int;
    static var SO_RCVLOWAT:Int;
    static var SO_RCVTIMEO:Int;
    static var SO_REUSEADDR:Int;
    static var SO_REUSEPORT:Int;
    static var SO_SNDBUF:Int;
    static var SO_SNDLOWAT:Int;
    static var SO_SNDTIMEO:Int;
    static var SO_TYPE:Int;
    static var TCP_CORK:Int;
    static var TCP_DEFER_ACCEPT:Int;
    static var TCP_INFO:Int;
    static var TCP_KEEPCNT:Int;
    static var TCP_KEEPIDLE:Int;
    static var TCP_KEEPINTVL:Int;
    static var TCP_LINGER2:Int;
    static var TCP_MAXSEG:Int;
    static var TCP_NODELAY:Int;
    static var TCP_QUICKACK:Int;
    static var TCP_SYNCNT:Int;
    static var TCP_WINDOW_CLAMP:Int;
    static var TIPC_ADDR_ID:Int;
    static var TIPC_ADDR_NAME:Int;
    static var TIPC_ADDR_NAMESEQ:Int;
    static var TIPC_CFG_SRV:Int;
    static var TIPC_CLUSTER_SCOPE:Int;
    static var TIPC_CONN_TIMEOUT:Int;
    static var TIPC_CRITICAL_IMPORTANCE:Int;
    static var TIPC_DEST_DROPPABLE:Int;
    static var TIPC_HIGH_IMPORTANCE:Int;
    static var TIPC_IMPORTANCE:Int;
    static var TIPC_LOW_IMPORTANCE:Int;
    static var TIPC_MEDIUM_IMPORTANCE:Int;
    static var TIPC_NODE_SCOPE:Int;
    static var TIPC_PUBLISHED:Int;
    static var TIPC_SRC_DROPPABLE:Int;
    static var TIPC_SUBSCR_TIMEOUT:Int;
    static var TIPC_SUB_CANCEL:Int;
    static var TIPC_SUB_PORTS:Int;
    static var TIPC_SUB_SERVICE:Int;
    static var TIPC_TOP_SRV:Int;
    static var TIPC_WAIT_FOREVER:Int;
    static var TIPC_WITHDRAWN:Int;
    static var TIPC_ZONE_SCOPE:Int;
    static var _GLOBAL_DEFAULT_TIMEOUT:Int;
}
