import type { StringStream } from '@codemirror/language';

import { StreamLanguage } from '@codemirror/language';
import { tags } from '@lezer/highlight';
import { createTheme } from '@uiw/codemirror-themes';

export const theme = createTheme({
  theme: 'dark',
  settings: {
    fontFamily: 'var(--font-mono)',
    background: 'var(--background)',
    foreground: 'var(--foreground)',
    caret: 'var(--foreground)',
    selection: 'color-mix(in oklch, var(--foreground) 30%, transparent)',
    selectionMatch: 'color-mix(in oklch, var(--foreground) 20%, transparent)',
    lineHighlight: 'color-mix(in oklch, var(--muted) 60%, transparent)',
    gutterBackground: 'var(--background)',
    gutterForeground: 'var(--muted-foreground)',
  },
  styles: [
    {
      tag: tags.keyword,
      color: 'color-mix(in oklch, var(--chart-1) 75%, purple)',
    },
    { tag: tags.literal, color: 'var(--foreground)' },
    { tag: tags.variableName, color: 'var(--foreground)' },
    { tag: tags.punctuation, color: 'var(--muted-foreground)' },
    { tag: tags.comment, color: 'var(--muted-foreground)' },
    {
      tag: tags.controlKeyword,
      color: 'color-mix(in oklch, var(--chart-2) 95%, white)',
    },
  ],
});

const KEYWORDS = new Set(['def', 'case', 'let', 'in']);

export function extension() {
  return StreamLanguage.define({
    name: 'lang',

    startState: () => ({}),

    token: (stream: StringStream) => {
      if (stream.eatSpace()) {
        return null;
      }

      if (stream.match(/^--.*/)) {
        return 'comment';
      }

      if (stream.match(/^[a-zA-Z_][a-zA-Z0-9_]*/)) {
        const name = stream.current();

        if (KEYWORDS.has(name)) {
          return 'keyword';
        }

        return 'variableName';
      }

      if (stream.match(/^->/) || stream.match(/^[:|=(){}[\],]/)) {
        return 'punctuation';
      }

      if (stream.match(/^\d+/)) {
        return 'literal';
      }

      stream.next();
      return null;
    },
  });
}
