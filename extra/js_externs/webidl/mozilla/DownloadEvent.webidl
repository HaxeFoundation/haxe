/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[Constructor(DOMString type, optional DownloadEventInit eventInitDict),
 Pref="dom.mozDownloads.enabled",
 CheckPermissions="downloads"]
interface DownloadEvent : Event
{
  readonly attribute DOMDownload? download;
};

dictionary DownloadEventInit : EventInit
{
  DOMDownload? download = null;
};
