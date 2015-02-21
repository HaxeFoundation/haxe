/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/fxtf/web-animations/#the-animation-interface
 *
 * Copyright © 2014 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Func="nsDocument::IsWebAnimationsEnabled"]
interface Animation {
  // FIXME: |effect| should have type (AnimationEffect or EffectCallback)?
  // but we haven't implemented EffectCallback yet.
  [Cached,Pure]
  readonly attribute AnimationEffect? effect;
  // FIXME: This should be writeable (bug 1067769)
  readonly attribute Element?         target;
};
