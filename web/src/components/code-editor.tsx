import type { EditorView, Extension, ViewUpdate } from '@uiw/react-codemirror';

import CodeMirror from '@uiw/react-codemirror';

type CodeEditorProps = {
  code: string;
  onCodeChange: (code: string) => void;
  extensions: Extension[];
  theme: Extension;
  onCreateEditor: (view: EditorView) => void;
  onUpdate: (update: ViewUpdate) => void;
};

export function CodeEditor({
  code,
  onCodeChange,
  extensions,
  theme,
  onCreateEditor,
  onUpdate,
}: CodeEditorProps) {
  return (
    <CodeMirror
      className="grow"
      theme={theme}
      value={code}
      extensions={extensions}
      onChange={onCodeChange}
      onCreateEditor={onCreateEditor}
      onUpdate={onUpdate}
    />
  );
}
