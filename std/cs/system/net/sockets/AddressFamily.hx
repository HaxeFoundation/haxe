
package cs.system.net.sockets;

@:native("System.Net.Sockets.AddressFamily")
extern enum AddressFamily {
	Unknown; // Unbekannte Adressfamilie. 
	Unspecified; // Nicht definierte Adressfamilie. 
	Unix; // UNIX-Hostadresse. 
	InterNetwork; // Adresse für IP Version 4. 
	ImpLink; // ARPANET IMP-Adresse. 
	Pup; // Adresse für PUP-Protokolle. 
	Chaos; // Adresse für MIT CHAOS-Protokolle. 
	NS; // Adresse für Xerox NS-Protokolle. 
	Ipx; // IPX- oder SPX-Adresse. 
	Iso; // Adresse für ISO-Protokolle. 
	Osi; // Adresse für ISO-Protokolle. 
	Ecma; // ECMA-Adresse (European Computer Manufacturers Association). 
	DataKit; // Adresse für Datakit-Protokolle. 
	Ccitt; // Adressen für CCITT-Protokolle, z. B. X.25. 
	Sna; // IBM SNA-Adresse. 
	DecNet; // DECnet-Adresse. 
	DataLink; // Adresse der Direct Data Link-Schnittstelle. 
	Lat; // LAT-Adresse. 
	HyperChannel; // NSC Hyperchannel-Adresse. 
	AppleTalk; // AppleTalk-Adresse. 
	NetBios; // NetBios-Adresse. 
	VoiceView; // VoiceView-Adresse. 
	FireFox; // FireFox-Adresse. 
	Banyan; // Banyan-Adresse. 
	Atm; // Systemeigene Adresse für ATM-Dienste. 
	InterNetworkV6; // Adresse für IP Version 6. 
	Cluster; // Adresse für Microsoft Cluster-Produkte. 
	Ieee12844; // Adresse der IEEE 1284.4-Arbeitsgruppe. 
	Irda; // IrDA-Adresse. 
	NetworkDesigners; // Adresse für Network Designers OSI-Gateway-fähige Protokolle. 
	Max; //MAX-Adresse. 
}
