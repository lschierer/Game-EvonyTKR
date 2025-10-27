import{directive as i}from"lit/directive.js";import{AsyncDirective as t}from"lit/async-directive.js";import{Signal as s}from"signal-polyfill";
/**
 * @license
 * Copyright 2023 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */class h extends t{_$Sl(){if(void 0!==this._$Su)return;this._$SW=new s.Computed((()=>{var i;return null===(i=this._$Sj)||void 0===i?void 0:i.get()}));const i=this._$Su=new s.subtle.Watcher((()=>{var t;null===(t=this._$SO)||void 0===t||t._(this),i.watch()}));i.watch(this._$SW)}_$Sp(){var i;void 0!==this._$Su&&(this._$Su.unwatch(this._$SW),this._$SW=void 0,this._$Su=void 0,null===(i=this._$SO)||void 0===i||i.m(this))}commit(){this.setValue(s.subtle.untrack((()=>{var i;return null===(i=this._$SW)||void 0===i?void 0:i.get()})))}render(i){return s.subtle.untrack((()=>i.get()))}update(i,[t]){var h,o;return null!==(h=this._$SO)&&void 0!==h||(this._$SO=null===(o=i.options)||void 0===o?void 0:o.host),t!==this._$Sj&&void 0!==this._$Sj&&this._$Sp(),this._$Sj=t,this._$Sl(),s.subtle.untrack((()=>this._$SW.get()))}disconnected(){this._$Sp()}reconnected(){this._$Sl()}}const o=i(h);export{h as WatchDirective,o as watch};
//# sourceMappingURL=watch.js.map
