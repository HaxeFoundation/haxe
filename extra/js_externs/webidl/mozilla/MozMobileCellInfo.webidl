/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/. */

[Pref="dom.mobileconnection.enabled"]
interface MozMobileCellInfo
{
  /**
   * Mobile Location Area Code (LAC) for GSM/WCDMA networks.
   *
   * Possible ranges from 0x0000 to 0xffff.
   * -1 if the LAC is unknown.
   */
  readonly attribute long gsmLocationAreaCode;

  /**
   * Mobile Cell ID for GSM/WCDMA networks.
   *
   * Possible ranges from 0x00000000 to 0xffffffff.
   * -1 if the cell id is unknown.
   */
  readonly attribute long long gsmCellId;

  /**
   * Base Station ID for CDMA networks.
   *
   * Possible ranges from 0 to 65535.
   * -1 if the base station id is unknown.
   */
  readonly attribute long cdmaBaseStationId;

  /**
   * Base Station Latitude for CDMA networks.
   *
   * Possible ranges from -1296000 to 1296000.
   * -2147483648 if the latitude is unknown.
   *
   * @see 3GPP2 C.S0005-A v6.0.
   */
  readonly attribute long cdmaBaseStationLatitude;

  /**
   * Base Station Longitude for CDMA networks.
   *
   * Possible ranges from -2592000 to 2592000.
   * -2147483648 if the longitude is unknown.
   *
   * @see 3GPP2 C.S0005-A v6.0.
   */
  readonly attribute long cdmaBaseStationLongitude;

  /**
   * System ID for CDMA networks.
   *
   * Possible ranges from 0 to 32767.
   * -1 if the system id is unknown.
   */
  readonly attribute long cdmaSystemId;

  /**
   * Network ID for CDMA networks.
   *
   * Possible ranges from 0 to 65535.
   * -1 if the network id is unknown.
   */
  readonly attribute long cdmaNetworkId;
};
