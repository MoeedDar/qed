import { LanguageServerConnection } from '@/lib/lsp';

import type { Diagnostic } from '@/lib/lsp';

declare global {
  interface Window {
    qed?: {
      ping: () => string;
      check: (text: string) => {
        diagnostics: {
          start: { line: number; character: number };
          end: { line: number; character: number };
          severity?: number;
          message: string;
        }[];
      };
      hover: (text: string, line: number, col: number) => { markdown: string };
      goals: (
        text: string,
        line: number,
        col: number,
      ) => {
        goals: QedGoal[];
      };
    };
  }
}

export type QedHypothesis = {
  name: string;
  typ: string;
};

export type QedGoal = {
  start: { line: number; character: number };
  end: { line: number; character: number };
  context: QedHypothesis[];
  goal: string;
};

const COMPLETIONS = ['def', 'case', 'let', 'in'].map((label) => ({
  label,
  kind: 3,
}));

const problemsListeners = new Set<(diagnostics: Diagnostic[]) => void>();

function qedDiagnostics(text: string): Diagnostic[] {
  const result = window.qed?.check(text);

  if (result) {
    return result.diagnostics.map((d) => ({
      severity: d.severity ?? 1,
      range: { start: d.start, end: d.end },
      message: d.message,
    }));
  }

  return [];
}

function diagnose(text: string): Diagnostic[] {
  const diagnostics = qedDiagnostics(text);

  for (const listener of problemsListeners) {
    listener(diagnostics);
  }

  return diagnostics;
}

export function qedGoals(text: string, line: number, col: number): QedGoal[] {
  const result = window.qed?.goals(text, line, col);
  return result?.goals ?? [];
}

export class QedLSP extends LanguageServerConnection {
  constructor() {
    super({
      capabilities: {
        textDocumentSync: 1,
        hoverProvider: true,
        completionProvider: { triggerCharacters: ['.'] },
      },

      onOpen(_uri: string, text: string) {
        return diagnose(text);
      },

      onChange(_uri: string, text: string) {
        return diagnose(text);
      },

      onHover(text: string, line: number, character: number) {
        const result = window.qed?.hover(text, line, character);

        if (result?.markdown) {
          return { contents: { kind: 'markdown', value: result.markdown } };
        }
      },

      completions: COMPLETIONS,
    });
  }

  subscribeProblems(listener: (diagnostics: Diagnostic[]) => void) {
    problemsListeners.add(listener);

    return () => {
      problemsListeners.delete(listener);
    };
  }
}
