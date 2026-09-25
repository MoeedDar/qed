import { useEffect, useMemo, useRef, useState } from 'react';
import { LSPClient, languageServerExtensions } from '@codemirror/lsp-client';
import { vim as vimExtension } from '@replit/codemirror-vim';
import { Decoration, EditorView, RangeSet } from '@uiw/react-codemirror';
import readOnlyRangesExtension from 'codemirror-readonly-ranges';
import { useLocalstorageState } from 'rooks';

import { QedLSP, qedGoals } from '@/lib/qed-lsp';
import { extension, theme } from '@/utils/code';

import type { EditorState, ViewUpdate } from '@uiw/react-codemirror';
import type { Diagnostic } from '@/lib/lsp';
import type { QedGoal } from '@/lib/qed-lsp';

const EDITOR_URI = 'file:///playground.qed';

function statementRange(state: EditorState) {
  const first = state.doc.line(1);
  return [{ from: first.from, to: Math.min(first.to + 1, state.doc.length) }];
}

const statementMark = Decoration.mark({ class: 'cm-statement' });

function statementHighlight(state: EditorState) {
  const [range] = statementRange(state);
  return Decoration.set(statementMark.range(range.from, range.to));
}

function fixedCursor(view: EditorView) {
  const [range] = statementRange(view.state);
  return RangeSet.of(statementMark.range(range.from, range.to));
}

const fixedExtensions = [
  readOnlyRangesExtension(statementRange),
  EditorView.decorations.compute([], statementHighlight),
  EditorView.atomicRanges.of(fixedCursor),
];

type Position = { line: number; character: number };

function createLSP() {
  const extensions = languageServerExtensions();
  const client = new LSPClient({ extensions });
  const server = new QedLSP();

  client.connect(server);

  return { client, server };
}

const lspInfo = createLSP();

function cursorPosition(view: EditorView): Position {
  const offset = view.state.selection.main.head;
  const line = view.state.doc.lineAt(offset);

  return { line: line.number - 1, character: offset - line.from };
}

function contains(goal: QedGoal, position: Position) {
  const afterStart =
    position.line > goal.start.line ||
    (position.line === goal.start.line &&
      position.character >= goal.start.character);
  const beforeEnd =
    position.line < goal.end.line ||
    (position.line === goal.end.line &&
      position.character <= goal.end.character);

  return afterStart && beforeEnd;
}

function goalIndex(goals: QedGoal[], position: Position) {
  const match = goals.findIndex((goal) => contains(goal, position));
  return match >= 0 ? match : 0;
}

function refreshGoals(
  view: EditorView | null,
  setGoals: (g: QedGoal[]) => void,
  setCurrent: (i: number) => void,
) {
  if (view) {
    const position = cursorPosition(view);
    const nextGoals = qedGoals(
      view.state.doc.toString(),
      position.line,
      position.character,
    );
    setGoals(nextGoals);
    setCurrent(goalIndex(nextGoals, position));
  }
}

export function useEditorState(theorem = '') {
  const [code, setCode] = useState(theorem);
  const [seed, setSeed] = useState(theorem);
  const [vim, setVim] = useLocalstorageState('vim', false);
  const [goals, setGoals] = useState<QedGoal[]>([]);
  const [current, setCurrent] = useState(0);
  const [problems, setProblems] = useState<Diagnostic[]>([]);
  const [activePanel, setActivePanel] = useState<'goals' | 'problems' | null>(
    'goals',
  );
  const viewRef = useRef<EditorView | null>(null);

  if (seed !== theorem) {
    setSeed(theorem);
    setCode(theorem);
  }

  useEffect(() => lspInfo.server.subscribeProblems(setProblems), []);

  const extensions = useMemo(
    () => [
      lspInfo.client.plugin(EDITOR_URI),
      extension(),
      theorem ? fixedExtensions : [],
      EditorView.lineWrapping,
      vim ? vimExtension() : [],
    ],
    [vim, theorem],
  );

  function onCreateEditor(view: EditorView) {
    viewRef.current = view;
  }

  function onUpdate(update: ViewUpdate) {
    if (update.docChanged || update.selectionSet) {
      refreshGoals(viewRef.current, setGoals, setCurrent);
    }
  }

  function jumpTo(line: number, character: number) {
    const view = viewRef.current;
    if (view) {
      const l = view.state.doc.line(line + 1);
      const offset = Math.min(l.to, l.from + character);
      view.dispatch({
        selection: { anchor: offset },
        scrollIntoView: true,
      });
      view.focus();
    }
  }

  function jumpToPosition(position: Position) {
    const view = viewRef.current;
    if (view) {
      const line = view.state.doc.line(position.line + 1);
      const offset = Math.min(line.to, line.from + position.character);
      view.dispatch({
        selection: { anchor: offset },
        scrollIntoView: true,
      });
      view.focus();
    }
  }

  const toggleGoals = (pressed: boolean) =>
    setActivePanel(pressed ? 'goals' : null);

  const toggleProblems = (pressed: boolean) =>
    setActivePanel(pressed ? 'problems' : null);

  return {
    code,
    setCode,
    vim,
    setVim,
    goals,
    current,
    problems,
    activePanel,
    showPanel: activePanel !== null,
    toggleGoals,
    toggleProblems,
    extensions,
    theme,
    onCreateEditor,
    onUpdate,
    jumpTo,
    jumpToPosition,
  };
}
