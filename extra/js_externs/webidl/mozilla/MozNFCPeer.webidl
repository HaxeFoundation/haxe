/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Part of this IDL file is from:
 * http://w3c.github.io/nfc/proposals/common/nfc.html#idl-def-NFCPeer
 *
 * Copyright © 2013 Deutsche Telekom, Inc.
 */

[JSImplementation="@mozilla.org/nfc/NFCPeer;1", AvailableIn="PrivilegedApps"]
interface MozNFCPeer {
  /**
   * Send NDEF data to peer device.
   */
  [Throws]
  Promise<void> sendNDEF(sequence<MozNDEFRecord> records);

  /**
   * Send file to peer device.
   */
  [Throws, CheckPermissions="nfc-share", AvailableIn="CertifiedApps"]
  Promise<void> sendFile(Blob blob);
};

// Mozilla Only
partial interface MozNFCPeer {
  [ChromeOnly]
  attribute DOMString session;

  /**
   * Indicate if this peer is already lost.
   */
  readonly attribute boolean isLost;
};
