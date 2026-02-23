// /js/lib/partials/global_context.js
import { ContextRoot } from '@lit/context';

new ContextRoot().attach(document.body); // attach once

interface ContextRequestEvent extends Event {
  composedPath(): EventTarget[];
  detail?: {
    context?: unknown;
    callback?: unknown;
    subscribe?: unknown;
  };
}

window.addEventListener(
  'context-request',
  (e) => {
    const ev = e as ContextRequestEvent;
    // ev.detail is { context, callback, subscribe }
    console.log(
      '[GLOBAL] context-request',
      'origin:',
      ev.composedPath()[0],
      'context:',
      ev.detail?.context, // <-- THIS will now show the key
    );
  },
  { capture: true },
);
