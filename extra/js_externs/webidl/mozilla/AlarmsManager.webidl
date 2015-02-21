/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * https://wiki.mozilla.org/WebAPI/AlarmAPI
 */

[NavigatorProperty="mozAlarms",
 JSImplementation="@mozilla.org/alarmsManager;1",
 CheckPermissions="alarms",
 Pref="dom.mozAlarms.enabled"]
interface AlarmsManager {
  DOMRequest getAll();
  DOMRequest add(any date, DOMString respectTimezone, optional any data);
  void remove(unsigned long id);
};
